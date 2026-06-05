% ============================================================================
% CONSTRAINT STORY: marriage_formation_collapse
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_formation_collapse, []).

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
 *   constraint_id: marriage_formation_collapse
 *   human_readable: Marriage Formation Collapse in Late Modernity
 *   domain: social/demographic/economic
 *
 * SUMMARY:
 *   Marriage formation in late modernity exhibits structural collapse across
 *   developed economies: median age at first marriage has risen from 23
 *   (1980) to 30+ (2023); proportion never-married by age 40 has doubled;
 *   fertility rates have fallen below replacement in most OECD nations. This
 *   constraint manifests as a tangled rope where genuine coordination
 *   function (marriage enables resource pooling, risk sharing, reproductive
 *   stability) coexists with systematic extraction that makes marriage
 *   economically inaccessible to the young adults who would most benefit from
 *   it. The extraction mechanisms operate through three channels: (1) housing
 *   speculation that has decoupled shelter from wage-earning capacity, (2)
 *   education financialization that burdens cohorts with 15-30 year debt
 *   servicing obligations, (3) childcare privatization that locks household
 *   income into care costs, reducing capital available for marriage-enabling
 *   savings. The constraint is simultaneously a natural consequence of
 *   demographic transition (increased female autonomy, reproductive control,
 *   delayed maturation of labor-market entry) and an actively enforced
 *   institutional arrangement (via finance sector profit models, real estate
 *   speculation, and education debt securitization). The middle-income
 *   household experiences this as tangled rope — marriage provides genuine
 *   coordination benefits but is systematically deferred until late 30s/early
 *   40s when reproductive windows narrow and family formation becomes
 *   high-risk. The young adult trapped in this system experiences it as snare
 *   — structural barriers make marriage formation impossible during
 *   biological windows. The theater ratio (0.48) reflects that ceremonial and
 *   romantic aspects of marriage persist at high intensity even as the
 *   institutional foundation has collapsed — substantial cultural energy
 *   invested in 'finding the one' and weddings, with declining functional
 *   coordination of economic life or family formation.
 *
 * KEY AGENTS:
 *   - Young Adults Aged 18-35: Primary victims (powerless/trapped) — face cumulative structural barriers (housing unaffordability, student debt, wage stagnation, childcare costs) that make marriage formation economically impossible during reproductive window
 *   - Housing Markets and Real Estate Finance: Primary beneficiaries (institutional/arbitrage) — capture sustained extraction through speculation-driven price inflation and mortgage debt securitization; benefit from indefinite deferral of first-home purchase
 *   - Consumer Financial Sector (Student Loans, Credit Cards): Secondary beneficiaries (institutional/arbitrage) — sustain debt service obligations that prevent capital accumulation; profit from refinancing and interest payments
 *   - Middle-Income Aspiring Households: Mixed agent (moderate/constrained) — experience both genuine coordination function (marriage enables dual-income household) and extraction (forced delay, reduced fertility)
 *   - Educational Institutions: Beneficiaries (institutional/arbitrage) — capture substantial fees and debt financing; benefit from deferred household formation (students remain longer in credential-seeking mode)
 *   - Childcare Sector and Privatized Caregiving: Beneficiaries (institutional/arbitrage) — profit from privatized childcare costs that prevent household capital formation
 *   - Housing Reform and Family Policy Coalition: Organized agents (organized/constrained) — advocate for zoning reform, housing cost reduction, childcare subsidies, and wage policy; see marriage formation collapse as solvable through structural policy change with generational sunset logic
 *   - The Cultural Institution of Marriage: Institutional actor (institutional/arbitrage) — maintains ceremonial and romantic functions (piton perspective) despite atrophied coordination function; theater persists as institutions invest in wedding industry, romance narratives, relationship counseling
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional arrangements as inevitable demographic transition; falsely summits as mountain if policy choices are treated as laws of nature
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_formation_collapse, 0.58).
domain_priors:suppression_score(marriage_formation_collapse, 0.62).
domain_priors:theater_ratio(marriage_formation_collapse, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_formation_collapse, extractiveness, 0.58).
narrative_ontology:constraint_metric(marriage_formation_collapse, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(marriage_formation_collapse, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_formation_collapse, tangled_rope).
narrative_ontology:human_readable(marriage_formation_collapse, "Marriage Formation Collapse in Late Modernity").
narrative_ontology:topic_domain(marriage_formation_collapse, "social/demographic/economic").

domain_priors:requires_active_enforcement(marriage_formation_collapse).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_formation_collapse, housing_markets).
narrative_ontology:constraint_beneficiary(marriage_formation_collapse, consumer_financial_sector).
narrative_ontology:constraint_beneficiary(marriage_formation_collapse, educational_institutions).
narrative_ontology:constraint_victim(marriage_formation_collapse, young_adults_aged_18_35).
narrative_ontology:constraint_victim(marriage_formation_collapse, reproductive_autonomy).
narrative_ontology:constraint_victim(marriage_formation_collapse, community_stability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: YOUNG ADULT COHORT (SNARE) — Trapped by structural economic barriers: housing unaffordability, student debt burden, wage stagnation, childcare costs. Marriage formation requires accumulated capital (down payment, stable income, housing security) that systematic extraction through education loans and housing speculation makes impossible. No exit pathway within the biographical horizon — deferral of family formation is structural, not chosen.
constraint_indexing:constraint_classification(marriage_formation_collapse, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MIDDLE-INCOME ASPIRING HOUSEHOLD (TANGLED ROPE) — Constrained by high costs (housing, education, childcare) but experiences genuine coordination function: marriage pools resources, shares risks, enables both partners' career development. The constraint is simultaneously extractive (forced delay, reduced family size) and coordinative (partnership enables economic mobility that individual cannot achieve). Exit through geographical relocation is possible but costly.
constraint_indexing:constraint_classification(marriage_formation_collapse, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: FINANCE AND REAL ESTATE SECTOR (ROPE) — Benefits from marriage delay through sustained debt servicing and asset inflation. Experiences the constraint as coordination: mortgage debt enables household asset accumulation, which anchors consumer spending and financial system stability. The sector has full exit option (can move capital elsewhere) and captures arbitrage through speculative appreciation and transaction fees.
constraint_indexing:constraint_classification(marriage_formation_collapse, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: SOCIAL INSTITUTION OF MARRIAGE (PITON) — Marriage as a ceremony and legal status persists through cultural and religious inertia, but its primary coordination function (economic pooling, reproductive stability, social status) has atrophied. Theater_ratio is moderate-high: substantial performative emphasis on 'finding the one' and ceremonial aspects, with declining functional coordination as couples delay formation until late 30s or skip entirely. The institution is maintained by expectation and ritual despite declining structural necessity.
constraint_indexing:constraint_classification(marriage_formation_collapse, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: HOUSING REFORM AND CHILDCARE COALITION (SCAFFOLD) — Organized movements (housing advocates, family policy reformers, universal childcare proponents) see marriage formation collapse as a temporary problem with structural sunset clauses: reducing housing costs through zoning reform, subsidizing childcare, and enforcing wage growth would remove the extraction layer without eliminating marriage as a coordination mechanism. These agents have agency and see explicit exit pathways through policy change within generational timescales.
constraint_indexing:constraint_classification(marriage_formation_collapse, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / CIVILIZATIONAL VIEW (MOUNTAIN) — From a long-view perspective, declining marriage formation rates reflect universal demographic transition: as societies increase female education, reproductive control, and economic participation, marriage shifts from economic necessity to optional social arrangement. This perspective risks naturalizing what are actually contingent policy choices (housing speculation, debt-financed education, childcare privatization) as inevitable civilizational evolution. The engine's false summit detector will identify this as naturalization requiring structural decomposition.
constraint_indexing:constraint_classification(marriage_formation_collapse, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_formation_collapse_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(marriage_formation_collapse, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(marriage_formation_collapse, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_formation_collapse, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(marriage_formation_collapse, TR),
    TR >= 0.70.

:- end_tests(marriage_formation_collapse_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint has accumulated substantially since 1980 (starting value 0.28, rising to 0.58 by 2023), driven by compounding housing price inflation, education debt expansion, and childcare cost growth. The extractiveness reflects that marriage formation has become impossible for the median young adult without either (a) substantial parental capital transfer, (b) geographic relocation to lower-cost region, or (c) abandonment of other life goals (education, career development, autonomy). However, extractiveness is not maximal (0.70+) because some coordination function remains genuine — marriage still enables dual-income household formation and risk pooling, and some agents do successfully form marriages despite barriers. Suppression (0.62): High. Structural barriers are substantial: median house price-to-income ratio has doubled since 1980; student debt outstanding is $1.7 trillion; childcare costs consume 15-25% of household income for median families. The suppression is primarily economic (not violent or overtly coercive) but is systematic and near-total for agents without parental wealth. Theater ratio (0.48): Moderate. The institution maintains high cultural and ceremonial energy (weddings, romance narratives, relationship counseling, wedding industry spending), but the functional coordination role (economic pooling, reproductive stability, social status) has substantially degraded because marriage is now deferred until financial conditions are met that rarely occur in the young adult window. The theater has not declined as steeply as the function because romantic and ceremonial aspects operate independently from the economic coordination that makes family formation possible.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap reveals that young adults and financial institutions experience the same constraint in opposite directions: one as snare (total extraction, no coordination), the other as rope (coordination with arbitrage benefit). This asymmetry is the diagnostic signal for tangled rope — genuine coordination (marriage enables household formation) is bundled with asymmetric extraction (forced deferral extracts from young adults and transfers reproductive capacity to late 30s/early 40s when fertility is declining and child-rearing window is compressed). The gap between the snare experienced by trapped young adults and the rope experienced by institutional beneficiaries is the widest possible perspectival gap, indicating high structural asymmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective is determined by the agent's position relative to the extraction flow. Young adults are pure targets: they bear costs (delayed family formation, reduced fertility, psychological distress) with no benefits from the constraint. Housing markets and financial sector are beneficiaries: they capture sustained extraction through speculation and debt servicing. Middle-income households occupy an intermediate position — they benefit from coordination (marriage enables household income pooling) but bear extraction costs (forced deferral, reduced family size). The housing reform coalition has organized power and exit pathways (policy advocacy, geographic relocation, alternative living arrangements), so their experienced extraction is moderate. The social institution of marriage has institutional power but no real exit option — it persists through inertia despite reduced function, producing piton classification. The analytical observer at civilizational scale risks high d (seeing extraction as natural) despite structural position as observer (d_analytical = 0.73). The directionality derives from beneficiary/victim declarations and exit option modulation per the standard chain: trapped young adults with no exit generate high d → high f(d) → high χ. Institutional beneficiaries with arbitrage options generate low d → low/negative χ.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY PRESENT BUT RESOLVABLE: The constraint exhibits genuine tensions between coordination and extraction that mandatrophy analysis must address. The coordination function is real: marriage does pool resources, share risks, enable household capital formation, and provide reproductive stability. The extraction is also real: the institutional arrangements (housing speculation, education debt, privatized childcare) systematically prevent young adults from accessing this coordination benefit. The mandatrophy is resolved by recognizing that marriage formation collapse represents a regime shift from one equilibrium (early marriage with lower individual mobility) to another equilibrium (late marriage with higher individual autonomy). The constraint is not a false positive (calling extraction coordination) or a false negative (calling coordination extraction). Rather, it is a state transition: the coordination function of marriage persists, but institutional arrangements have reorganized to extract from those who would access that function. The young adult experiences snare precisely because the extraction mechanisms prevent access to coordination. The housing sector experiences rope precisely because speculation generates both profits and coordination benefits (mortgages do enable household formation for those with sufficient capital). The resolution is that mandatrophy is not an error in classification but an accurate reflection of structural heterogeneity: the same institutional arrangements produce different types (snare, rope, tangled rope) when evaluated from different structural positions. The engine's job is to detect and report this heterogeneity, not to collapse it into a single type.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    housing_cost_causal_threshold,
    'What proportion of marriage formation delay is directly caused by housing unaffordability versus other factors (education duration, female workforce participation, contraceptive access)?',
    'Comparative historical analysis (pre-1980 vs post-2000 housing cost-to-income ratios) and cross-national comparison (countries with housing price controls vs speculation-driven markets). Synthetic control matching young adults by income/education, varying housing cost scenarios.',
    'If housing is >60% causal: constraint is primarily economic extraction, and policy intervention (zoning reform, speculation taxes) would measurably increase marriage rates. If housing is <30% causal: delay is driven by cultural preference/expanded autonomy, and housing policy alone cannot reverse the trend.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(housing_cost_causal_threshold, empirical, 'Causal attribution of marriage delay to housing costs').

omega_variable(
    female_autonomy_preference_decomposition,
    'To what extent do young women defer marriage due to structural barriers (trapped, constrained) versus expanded preference for education/career/autonomy (mobile, arbitrage)?',
    'Survey data on marriage intentions vs actual behavior; qualitative interviews distinguishing ''cannot afford marriage'' from ''choosing other life paths''; cross-generational comparison of cohort attitudes toward marriage centrality.',
    'If autonomy preference dominates: the constraint is not extractive but rather a mismatch between old institutional forms and new values. Classification would shift from Snare toward Piton (degraded institution) for many perspectives. If barriers dominate: classification remains Snare/Tangled Rope with extraction flowing toward institutional beneficiaries.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(female_autonomy_preference_decomposition, empirical, 'Decomposition of preference vs structural barriers in marriage deferral').

omega_variable(
    childcare_coordination_function_vitality,
    'Does marriage-mediated childcare coordination (two-parent household) remain functionally superior to alternative arrangements (single parenthood, extended family, institutional childcare) given modern labor markets and social support?',
    'Longitudinal outcomes comparison: child development, economic stability, parental wellbeing across household structures, controlling for income/education. Analysis of time-use patterns and task-sharing efficiency in two-parent vs other arrangements.',
    'If marriage remains coordinatively superior: the constraint is extractive precisely because it blocks a genuinely more efficient arrangement. If alternative arrangements are equivalent: marriage is purely performative/cultural, and the collapse is reclassification from coordination to degradation (Piton intensification).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(childcare_coordination_function_vitality, empirical, 'Whether marriage provides coordinative advantage for childcare vs alternatives').

omega_variable(
    identity_lock_mechanism_in_family_planning,
    'Are young adults trapped by material barriers (housing, debt, childcare cost) or identity-locked through internalized narratives (belief they must achieve certain milestones before ''deserving'' family, fusion of identity with educational/career achievement)?',
    'Qualitative analysis distinguishing expressed barriers from internalized constraints; comparison of self-reported reasons for delay; observation of behavior when constraints are partially removed (housing subsidy experiment, education debt forgiveness) — do marriage rates recover or remain low due to identity reorientation?',
    'If identity-locked dominates: the constraint persists even after material barriers are removed. Agents would need cognitive reframing, not just policy change. Classification from identity-locked perspective becomes Rope (changeable in principle if identity frame shifts) rather than Mountain (unchangeable). This implies therapeutic or cultural intervention is primary solution.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_mechanism_in_family_planning, conceptual, 'Decomposition of material vs identity-based barriers to marriage formation').

omega_variable(
    institutional_beneficiary_intentionality,
    'Do housing speculation, student debt finance, and consumer credit expansion constitute deliberate extraction mechanism designed to prevent marriage formation, or are they incidental consequences of other profit-driven policies?',
    'Historical analysis of policy intent (housing zoning, education privatization, debt securitization); examination of institutional actors'' explicit awareness of marriage formation effects; comparison to scenarios where same profit opportunities existed with policies protecting marriage formation.',
    'If intentional: the constraint is a snare sustained by deliberate institutional suppression. If incidental: it is a tangled rope where genuine coordination (mortgage financing, education access) carries extraction as side effect. This affects mandatrophy analysis — intentional extraction requires different remedies than systems optimization.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(institutional_beneficiary_intentionality, preference, 'Whether institutional extraction from marriage formation is intentional or incidental').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_formation_collapse, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mfc_tr_t0, marriage_formation_collapse, theater_ratio, 0, 0.32).
narrative_ontology:measurement(mfc_tr_t15, marriage_formation_collapse, theater_ratio, 15, 0.4).
narrative_ontology:measurement(mfc_tr_t30, marriage_formation_collapse, theater_ratio, 30, 0.48).
narrative_ontology:measurement(mfc_tr_t10, marriage_formation_collapse, theater_ratio, 10, 0.36).

% Extraction over time
narrative_ontology:measurement(mfc_be_t0, marriage_formation_collapse, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(mfc_be_t15, marriage_formation_collapse, base_extractiveness, 15, 0.42).
narrative_ontology:measurement(mfc_be_t30, marriage_formation_collapse, base_extractiveness, 30, 0.58).
narrative_ontology:measurement(mfc_be_t10, marriage_formation_collapse, base_extractiveness, 10, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_formation_collapse, resource_allocation).
narrative_ontology:affects_constraint(marriage_formation_collapse, housing_speculation_mechanism).
narrative_ontology:affects_constraint(marriage_formation_collapse, education_debt_financialization).
narrative_ontology:affects_constraint(marriage_formation_collapse, childcare_cost_privatization).
narrative_ontology:affects_constraint(marriage_formation_collapse, wage_stagnation_relative_cost_growth).

% DUAL FORMULATION NOTE:
% Marriage formation collapse is downstream of three distinct institutional extraction mechanisms (housing, education, childcare). This constraint story represents the aggregated effect at the social institution level. Upstream stories decompose the specific mechanisms: housing_speculation_mechanism (ε=0.68), education_debt_financialization (ε=0.72), childcare_cost_privatization (ε=0.55). The collapse is the structural result when all three operate simultaneously on the same cohort. Network links establish this family relationship — all downstream constraints affect this constraint; this constraint affects downstream demographic and fertility outcomes.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(marriage_formation_collapse, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
