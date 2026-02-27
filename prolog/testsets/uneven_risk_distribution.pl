% ============================================================================
% CONSTRAINT STORY: uneven_risk_distribution
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_uneven_risk_distribution, []).

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
 *   constraint_id: uneven_risk_distribution
 *   human_readable: Uneven Distribution of Modifiable Health Risk Factors
 *   domain: public_health/epidemiology
 *
 * SUMMARY:
 *   The uneven distribution of modifiable health risk factors — smoking, HPV
 *   exposure, obesity, dietary inadequacy, physical inactivity — represents a
 *   persistent structural constraint embedded in epidemiological reality.
 *   This is not a law of nature or an inevitable feature of human biology,
 *   but rather the observable outcome of how economic stratification,
 *   healthcare access, workplace conditions, housing quality, and food
 *   systems are institutionally organized. Low-income populations experience
 *   concentrated exposure to these risk factors not primarily through
 *   individual behavior differences but through constrained material
 *   conditions: they cannot afford smoke-free housing, cannot take time from
 *   work for vaccination appointments, cannot purchase nutritionally adequate
 *   food on limited budgets, and lack safe spaces for physical activity.
 *   Meanwhile, high-income populations benefit from the gradient: they gain
 *   access to preventive interventions, personalized medicine targeting, and
 *   pharmaceutical treatments for risk-factor-derived disease. The constraint
 *   exhibits classic tangled rope structure: public health systems coordinate
 *   genuine health improvement efforts while simultaneously enforcing
 *   individual-responsibility narratives that blame victims for their
 *   exposure. The pharmaceutical and healthcare industries benefit from the
 *   entire ecosystem through treatment markets. Structural reform movements
 *   (labor organizing, housing advocacy, community health centers) represent
 *   a scaffold with sunset logic — as material conditions improve, risk
 *   factor concentration naturally declines without requiring behavioral
 *   modification campaigns. The performative health messaging industry (diet
 *   culture, personal responsibility narratives, 'just say no') maintains
 *   institutional theater as actual risk distribution remains stable. The
 *   analytical observer risks naturalizing this entire institutional
 *   arrangement as an immutable statistical law of human nature.
 *
 * KEY AGENTS:
 *   - Low-Income Populations: Primary victim (powerless/trapped) — structurally forced into concentration of modifiable risk factors by economic constraints; bear full extraction burden
 *   - High-Income Populations: Primary beneficiary (institutional/arbitrage) — access preventive interventions and face lower risk exposure; experience constraint as positive coordination
 *   - Healthcare Industry: Secondary beneficiary (institutional/arbitrage) — market for treatments, diagnostic services, and risk-reduction products; profit from risk factor distribution maintained
 *   - Public Health Systems: Institutional coordinator (moderate/constrained) — balance genuine health coordination with enforcement of individual-responsibility blame narratives; active enforcement required
 *   - Structural Reform Movements: Organized agents (organized/constrained) — labor, housing, and community health advocates building material conditions that reduce risk factor concentration with sunset logic
 *   - Health Messaging Industry: Institutional performer (institutional/arbitrage) — maintain behavioral messaging campaigns with high theater ratio; function persists despite low population-level impact
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional arrangements as inevitable statistical laws
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(uneven_risk_distribution, 0.58).
domain_priors:suppression_score(uneven_risk_distribution, 0.65).
domain_priors:theater_ratio(uneven_risk_distribution, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(uneven_risk_distribution, extractiveness, 0.58).
narrative_ontology:constraint_metric(uneven_risk_distribution, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(uneven_risk_distribution, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(uneven_risk_distribution, tangled_rope).
narrative_ontology:human_readable(uneven_risk_distribution, "Uneven Distribution of Modifiable Health Risk Factors").
narrative_ontology:topic_domain(uneven_risk_distribution, "public_health/epidemiology").

domain_priors:requires_active_enforcement(uneven_risk_distribution).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(uneven_risk_distribution, high_income_populations).
narrative_ontology:constraint_beneficiary(uneven_risk_distribution, healthcare_industry).
narrative_ontology:constraint_beneficiary(uneven_risk_distribution, pharmaceutical_sector).
narrative_ontology:constraint_victim(uneven_risk_distribution, low_income_populations).
narrative_ontology:constraint_victim(uneven_risk_distribution, structural_health_equity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LOW-INCOME POPULATION (SNARE) — Structurally trapped by economic constraints that force concentration of modifiable risk factors. Cannot afford smoke-free housing, HPV vaccination, preventive care, or nutritionally adequate diet. Bears full extraction burden with zero organized exit capacity. Maximum experienced effective extraction.
constraint_indexing:constraint_classification(uneven_risk_distribution, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: PUBLIC HEALTH SYSTEMS (TANGLED ROPE) — Coordinate risk reduction education and vaccination programs (genuine coordination function) while simultaneously enforcing individual responsibility narratives that blame victims for risk factor exposure. Constrained by budget allocation and political pressure. Experience both coordination benefit and extraction cost. Active enforcement required to maintain the blame-the-victim framing.
constraint_indexing:constraint_classification(uneven_risk_distribution, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PHARMACEUTICAL AND HEALTHCARE INDUSTRY (ROPE) — Benefits from existing risk factor distribution through market for risk-reduction products (medications, treatments, diagnostic services). Gains arbitrage access to emerging markets and prevention interventions. Experiences constraint as coordination: risk factor surveillance data enables product development and market targeting. Primary beneficiary with full exit optionality.
constraint_indexing:constraint_classification(uneven_risk_distribution, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: STRUCTURAL REFORM MOVEMENTS (SCAFFOLD) — Organized agents (labor organizations, housing advocates, community health centers) work to establish material conditions that reduce risk factor concentration: living wages, safe housing, food access, workplace protections. These movements create alternative pathways that bypass individual-responsibility framing. Sunset logic applies: as structural conditions improve, the individual risk factor burden naturally declines without behavioral modification campaigns.
constraint_indexing:constraint_classification(uneven_risk_distribution, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: HEALTH EDUCATION AND BEHAVIORAL MESSAGING (PITON) — Risk reduction campaigns and personalized medicine messaging persist through institutional inertia despite evidence that individual behavior change has minimal effect on population-level risk distribution without structural change. The theater of 'just say no' and 'get vaccinated' narratives maintains institutional function while actual risk concentration remains stable. Theater ratio high because compliance with messaging tracks socioeconomic status, not message quality.
constraint_indexing:constraint_classification(uneven_risk_distribution, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / STATISTICAL INEVITABILITY (MOUNTAIN) — From the civilizational analytical view, uneven risk distribution appears as an immutable statistical law: economic stratification naturally produces correlated health behavior differences. But this naturalizes contingent institutional arrangements (healthcare financing, labor market structure, housing policy) as inevitable physical constraints. Engine's false summit detector will identify this as naturalization of manageable institutional design failures.
constraint_indexing:constraint_classification(uneven_risk_distribution, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(uneven_risk_distribution_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(uneven_risk_distribution, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(uneven_risk_distribution, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(uneven_risk_distribution, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(uneven_risk_distribution, TR),
    TR >= 0.70.

:- end_tests(uneven_risk_distribution_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High. The constraint structurally channels modifiable risk factors toward low-income populations while benefiting high-income populations and healthcare industries. The extraction has grown over the 60-year interval as healthcare costs have risen and low-wage work has become more precarious. Base extractiveness is not as severe as pure snare (≥0.66) because genuine coordination functions (public health campaigns, vaccination programs, clinical interventions) do reduce some absolute risk — the extraction is mixed with coordination. Suppression (0.65): High. Multiple layers of suppression maintain the constraint: economic barriers to risk modification, insufficient infrastructure for prevention, institutional messaging that blames individuals rather than addressing structural causes, and political resistance to redistribution of resources. Suppression is not total because some populations do exit through income mobility and some structural reforms succeed. Theater ratio (0.48): Moderate. Health messaging campaigns have significant performative content — compliance tracks socioeconomic status more than message quality — but they also achieve genuine behavioral change in affluent populations. The theater has declined over the interval as data on behavioral messaging efficacy has accumulated, but institutional momentum maintains campaigns despite modest population-level impact.
 *
 * PERSPECTIVAL GAP:
 *   The constraint produces maximum perspectival divergence because it operates at the intersection of material deprivation, institutional coordination, and market extraction. The powerless see snare; the institutional beneficiaries see rope; the organized advocates see scaffold; the performative systems see piton; the civilizational observer sees mountain. Each perspective is structurally justified — the constraint genuinely appears different from these positions. The gap is not due to measurement error but to genuine structural differences in how the agents experience the same institutional arrangement.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from structural position relative to extraction flow. Low-income populations are full targets (d ≈ 0.95): trapped exit + victim status → maximum experienced extraction. High-income populations and healthcare industries are beneficiaries (d ≈ 0.10-0.20): arbitrage exit + beneficiary status → low or negative experienced extraction. Public health systems are moderate victims (d ≈ 0.60): constrained exit + mixed beneficiary/victim status → moderate extraction. Structural reform movements have agency (d ≈ 0.45): constrained exit but organized power + clear beneficiary role → moderate extraction. The health messaging industry experiences minimal extraction (d ≈ 0.25-0.35) because it maintains institutional arbitrage access despite low functional impact. The piton classification derives from theater gate (0.48 is approaching 0.70 threshold) rather than from high experienced extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The constraint resolves mandatrophy by showing that Tangled Rope classification captures a genuinely mixed coordination-extraction structure. Public health systems do coordinate health improvement (genuine coordination function) while simultaneously enforcing individual-responsibility narratives that extract from low-income populations (asymmetric extraction). The active enforcement gate is satisfied by the institutional maintenance of blame narratives against countervailing structural evidence. The snare perspective from low-income populations reveals the extraction component; the rope perspective from healthcare industries reveals the coordination-as-profit-opportunity component. The scaffold perspective reveals that structural reform can decouple coordination from extraction — the sunset logic means that as material conditions improve, health improvement naturally follows WITHOUT requiring the extraction layer. This supports the tangled rope classification: the constraint is NOT pure coordination masquerading as extraction (false coordination), and it is NOT pure extraction with performative coordination (false coordination as cover). It is genuinely both, from different structural positions. The classification prevents the error of labeling individual-responsibility health messaging as 'pure coordination with no extraction component' (which would mask the class gradient) and prevents the opposite error of treating the public health coordination function as entirely theatrical (which would ignore genuine disease prevention outcomes).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    structural_vs_behavioral_decomposition,
    'What portion of observed risk factor concentration results from structural economic barriers versus from autonomous individual behavior choices?',
    'Natural experiments comparing identical populations with different economic access; intervention trials providing material resources independent of behavioral messaging',
    'If structural barriers dominate (>70%): constraint is primarily snare/tangled rope, individual-responsibility framing is false naturalization. If behavioral autonomy dominates (>40%): constraint has genuine rope component for individuals making informed choices.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(structural_vs_behavioral_decomposition, empirical, 'Decomposition of structural versus behavioral contributions to risk distribution').

omega_variable(
    healthcare_industry_incentive_alignment,
    'Does pharmaceutical/healthcare industry profit more from upstream prevention or from downstream treatment of risk-factor-derived disease?',
    'Market analysis of prevention vs treatment revenue streams; inverse correlation test: do health systems with highest prevention investment show lowest pharmaceutical revenue?',
    'If prevention dominates profit: industry beneficiary role is coordination-aligned, rope classification strengthened. If treatment dominates: industry has financial incentive to maintain risk factor distribution, snare/tangled rope classification confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(healthcare_industry_incentive_alignment, empirical, 'Industry profit incentive alignment with prevention versus treatment').

omega_variable(
    vaccination_access_sufficiency_threshold,
    'What level of free, accessible vaccination infrastructure is required for HPV/other risk-preventing interventions to reach saturation in low-income populations?',
    'Cross-national comparison of vaccination rates versus access infrastructure investment; saturation curve modeling from countries with universal access',
    'If low threshold (<$10 per capita annually): scaffold sunset is feasible, structural reform can solve the problem. If high threshold (>$100 per capita): even structural reform faces persistent barriers, constraint has deeper natural law component.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(vaccination_access_sufficiency_threshold, empirical, 'Threshold for vaccination saturation in low-income populations').

omega_variable(
    intergenerational_risk_persistence,
    'Does childhood exposure to parental risk factors (secondhand smoke, maternal obesity, food insecurity) lock in adult risk factor prevalence through epigenetic or developmental mechanisms?',
    'Longitudinal birth cohort studies tracking childhood exposure to adult risk behavior; comparison of adopted children with biological and adoptive family risk factors',
    'If lock-in is strong: scaffold sunset extends multi-generational, structural change required in childhood. If minimal: adult risk factors are more reversible, reform timelines shorter.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(intergenerational_risk_persistence, empirical, 'Intergenerational lock-in of risk factor exposure').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(uneven_risk_distribution, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(uneven_tr_t0, uneven_risk_distribution, theater_ratio, 0, 0.25).
narrative_ontology:measurement(uneven_tr_t30, uneven_risk_distribution, theater_ratio, 30, 0.35).
narrative_ontology:measurement(uneven_tr_t60, uneven_risk_distribution, theater_ratio, 60, 0.48).

% Extraction over time
narrative_ontology:measurement(uneven_be_t0, uneven_risk_distribution, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(uneven_be_t30, uneven_risk_distribution, base_extractiveness, 30, 0.45).
narrative_ontology:measurement(uneven_be_t60, uneven_risk_distribution, base_extractiveness, 60, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(uneven_risk_distribution, resource_allocation).
narrative_ontology:affects_constraint(uneven_risk_distribution, healthcare_access_stratification).
narrative_ontology:affects_constraint(uneven_risk_distribution, preventive_medicine_implementation_gap).
narrative_ontology:affects_constraint(uneven_risk_distribution, occupational_health_inequality).

% DUAL FORMULATION NOTE:
% Uneven risk distribution is downstream of multiple structural constraints (economic stratification, healthcare financing, labor market design, housing policy) but represents a distinct constraint in how these upstream factors combine into observable epidemiological patterns. The upstream constraints have their own extractiveness values reflecting policy design; this constraint captures the integrated health outcome gradient.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(uneven_risk_distribution, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
