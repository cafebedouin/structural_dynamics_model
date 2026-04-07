% ============================================================================
% CONSTRAINT STORY: sotu_1969_johnson_model_cities_program
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sotu_1969_johnson_model_cities_program, []).

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
 *   constraint_id: sotu_1969_johnson_model_cities_program
 *   human_readable: Model Cities Program: Federal Coordination of Urban Renewal (1969)
 *   domain: infrastructure/urban_development/federal_policy
 *
 * SUMMARY:
 *   The Model Cities program (1966-1977, expanded through 1980s) represents a
 *   critical inflection point in federal-local governance structure.
 *   President Johnson's vision was to replace fragmented municipal funding
 *   with coordinated federal planning and resource concentration in 150+
 *   designated urban centers. The program channels federal resources for
 *   integrated renewal of housing, schools, hospitals, and public facilities
 *   under federal blueprint supervision. Structurally, this creates a tangled
 *   hybrid: genuine coordination benefits (consolidated resources,
 *   professional planning capacity, infrastructure coherence) exist alongside
 *   asymmetric extraction (federal authority over local planning,
 *   displacement of residents, concentration of resources in designated
 *   cities at expense of others). The constraint's evolution over the
 *   interval (1969-1975) shows rising extractiveness as federal bureaucracy
 *   ossifies and contractor networks capture program benefits, while theater
 *   ratio increases as federal oversight becomes increasingly performative
 *   rather than functionally necessary.
 *
 * KEY AGENTS:
 *   - Federal Planning Apparatus: Primary beneficiary (institutional/arbitrage) — consolidates administrative authority and resource allocation control; gains planning power over 150+ cities
 *   - Participating Municipal Governments: Secondary beneficiary but constrained (institutional/constrained) — receives federal resources but loses planning autonomy; must conform to federal blueprints
 *   - Displaced Residents: Primary victim (powerless/trapped) — forced relocation without adequate compensation; no exit option; community dissolution
 *   - Non-Designated Municipalities: Secondary victim (organized/constrained) — excluded from federal resources; relative deprivation drives political grievance
 *   - Urban Renewal Contractors: Tertiary beneficiary with degradation (organized/constrained) — extract project profits under federal cost-plus contracts; oversight becomes theater over time
 *   - State Governments: Mixed position (institutional/constrained) — lose coordinating authority to federal-city direct relationship; some benefit from resource flow to their cities
 *   - Analytical Observer: Sees scaffold (analytical/analytical) — recognizes temporary coordination solution with embedded sunset logic if federal blueprint necessity declines
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sotu_1969_johnson_model_cities_program, 0.48).
domain_priors:suppression_score(sotu_1969_johnson_model_cities_program, 0.42).
domain_priors:theater_ratio(sotu_1969_johnson_model_cities_program, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sotu_1969_johnson_model_cities_program, extractiveness, 0.48).
narrative_ontology:constraint_metric(sotu_1969_johnson_model_cities_program, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(sotu_1969_johnson_model_cities_program, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sotu_1969_johnson_model_cities_program, tangled_rope).
narrative_ontology:human_readable(sotu_1969_johnson_model_cities_program, "Model Cities Program: Federal Coordination of Urban Renewal (1969)").
narrative_ontology:topic_domain(sotu_1969_johnson_model_cities_program, "infrastructure/urban_development/federal_policy").

domain_priors:requires_active_enforcement(sotu_1969_johnson_model_cities_program).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sotu_1969_johnson_model_cities_program, participating_municipal_governments).
narrative_ontology:constraint_beneficiary(sotu_1969_johnson_model_cities_program, urban_renewal_contractors).
narrative_ontology:constraint_beneficiary(sotu_1969_johnson_model_cities_program, federal_planning_apparatus).
narrative_ontology:constraint_victim(sotu_1969_johnson_model_cities_program, displaced_residents).
narrative_ontology:constraint_victim(sotu_1969_johnson_model_cities_program, local_political_autonomy).
narrative_ontology:constraint_victim(sotu_1969_johnson_model_cities_program, non_designated_municipalities).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DISPLACED RESIDENT (SNARE) — Urban renewal requires demolition of existing housing and commercial areas. Residents face forced relocation with inadequate compensation. No exit option: the constraint is embedded in their neighborhood. They bear maximum extraction cost (displacement, community dissolution, relocation burden) with minimal benefit. Federal coordination means local political remedies are blocked — appeals go to distant bureaucratic apparatus rather than local government.
constraint_indexing:constraint_classification(sotu_1969_johnson_model_cities_program, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: DESIGNATED CITY GOVERNMENT (TANGLED ROPE) — Receives substantial federal coordination benefits (concentrated resources, coordinated infrastructure, technical planning support) but loses autonomy over urban planning. Federal blueprint enforcement constrains local decision-making — cities must conform to federal priorities (highway corridors, downtown renewal) rather than community preferences. Constrained exit: leaving federal program means losing resources; staying means accepting federal planning supremacy. Mixed extraction and coordination.
constraint_indexing:constraint_classification(sotu_1969_johnson_model_cities_program, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: FEDERAL PLANNING APPARATUS (ROPE) — Pure coordination from federal standpoint: consolidating fragmented municipal funding into unified system solves coordination problem across 150+ jurisdictions. Federal administrative costs are recoverable through project efficiency gains. Can arbitrage between cities (shift resources based on federal priorities). Experiences constraint as coordination mechanism enabling their planning authority. Net beneficiary — gains administrative power and resource allocation authority.
constraint_indexing:constraint_classification(sotu_1969_johnson_model_cities_program, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: NON-DESIGNATED MUNICIPALITIES (TANGLED ROPE) — Organized at state/regional level but excluded from program benefits. Experience extraction through relative deprivation: federal resources concentrate in 150 designated cities while remaining 8,000+ municipalities compete for scarce remaining funding. Can exit through political pressure (advocacy, coalition-building) but at significant cost (reduced federal support for other programs, political alienation). Moderate extraction with some agency.
constraint_indexing:constraint_classification(sotu_1969_johnson_model_cities_program, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: URBAN RENEWAL CONTRACTORS (PITON) — Benefit from federal coordination (guaranteed contracts, stable resource flow) but the primary coordination function has degraded. Federal planning becomes less about solving genuine coordination problems and more about maintaining the apparatus itself — contractor eligibility, cost-plus compensation, federal oversight theater. By the 1970s, Model Cities contracts are distributed through political patronage and contractor networks rather than competitive allocation. High theater ratio reflects how federal oversight persists as ritualistic compliance rather than genuine quality control.
constraint_indexing:constraint_classification(sotu_1969_johnson_model_cities_program, piton,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: STATE GOVERNMENTS (TANGLED ROPE) — Federal program bypasses state structure entirely, channeling funds directly to cities. States experience loss of coordinating authority over urban development (extracted from state planning capacity). But states also benefit from federal resources flowing to cities within their borders, reducing state funding pressure. Constrained exit: can resist Model Cities participation but at cost of forgone federal resources. Federal-state relationship is asymmetric — federal offers resources with planning strings attached; states must choose between autonomy and funding.
constraint_indexing:constraint_classification(sotu_1969_johnson_model_cities_program, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (SCAFFOLD) — From civilizational distance, Model Cities is a temporary response to 1960s urban coordination crisis (fragmented funding, competing renewal efforts, unequal municipal capacity). The program embeds a sunset logic: as municipal governments gain capacity for coordinated planning, federal blueprint supervision can decline. Federal oversight becomes less necessary as cities develop internal planning infrastructure. Theater ratio rising (0.58) indicates the sunset is incomplete — bureaucratic perpetuation outlasts coordination necessity. But the analytical frame sees this as temporally bounded problem-solving mechanism rather than pure extraction.
constraint_indexing:constraint_classification(sotu_1969_johnson_model_cities_program, scaffold,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sotu_1969_johnson_model_cities_program_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sotu_1969_johnson_model_cities_program, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sotu_1969_johnson_model_cities_program, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(sotu_1969_johnson_model_cities_program, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(sotu_1969_johnson_model_cities_program, TR),
    TR >= 0.70.

:- end_tests(sotu_1969_johnson_model_cities_program_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48): Moderate-high, trending upward. Initial extractiveness (0.32 in 1969) reflects genuine coordination benefits during early implementation — consolidated resources do solve real coordination problems across fragmented municipal systems. By 1975 (0.52), extractiveness rises as federal bureaucracy grows, contractor rent-seeking increases, and coordination function increasingly supplanted by administrative empire-building. The upward trajectory shows displacement from coordination toward extraction as the program matures. Suppression (0.42): Moderate. Significant barriers to exit include federal funding dependency (cities cannot easily walk away without losing resources), displaced residents facing relocation without adequate political recourse, and state governments marginalized from urban planning. However, suppression is not total — municipal governments can pressure federal administration for modified blueprints, non-designated cities can lobby Congress, and contractors can seek alternative federal contracts. The constraint maintains itself through resource incentives and bureaucratic inertia rather than through complete coercion. Theater ratio (0.58): Moderate-high and rising. Federal oversight begins functionally (genuine planning coordination) but increasingly becomes performative: federal approval processes become checklist exercises, contractor selection devolves into political patronage, and blueprint enforcement becomes negotiated theater rather than real constraint. By 1975, much federal oversight is maintaining the apparatus rather than improving outcomes.
 *
 * PERSPECTIVAL GAP:
 *   Perspectives diverge sharply across power levels. Federal apparatus and participating city governments see coordination (rope, tangled rope with benefits). Displaced residents see pure extraction (snare) — the coordination benefits accrue to others; they bear only costs. Non-designated municipalities see extraction through exclusion (tangled rope, victim status). Urban renewal contractors see degraded function (piton) — the coordination logic that justified cost-plus contracts erodes into patronage system. State governments see their authority extracted (tangled rope) — federal-city direct relationship bypasses state role. The analytical observer can see both the genuine scaffold (temporary coordination solution) and the false summit logic (naturalizing federal control as technical necessity rather than political choice). This perspectival spread across all six types indicates a constraint at a critical point — the coordination function is real but the extraction mechanisms are intensifying.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary directionality: Federal planning apparatus derives d ≈ 0.05 (institutional power + arbitrage exit options → effective beneficiary → low d → negative χ). Participating cities derive d ≈ 0.35 (institutional power but constrained exit due to federal funding dependency → moderate extraction direction). Non-designated municipalities derive d ≈ 0.62 (organized power but mobile exit options and victim status due to exclusion → higher extraction direction). Displaced residents derive d ≈ 0.92 (powerless + trapped + victim status → maximum extraction direction). The directionality pipeline shows how the same constraint produces different effective extraction rates for different agents based on their structural relationship. Federal apparatus experiences negative extraction (coordination benefit); cities experience moderate extraction (resource gain with autonomy loss); excluded cities experience high extraction (relative deprivation); displaced residents experience maximum extraction (absolute loss without compensation). No directionality overrides needed — structural derivation captures the asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   The Model Cities program exemplifies how mandatrophy appears in federal-local governance. Early proponents argued the constraint is rope (pure coordination benefit — consolidating resources solves genuine municipal fragmentation problem). Critics argued it is snare (federal control masked as coordination — blueprint enforcement extracts local autonomy and displaces communities). The mandatrophy resolves through the tangled_rope classification: BOTH are correct. The constraint genuinely coordinates resources AND genuinely extracts from local governments and residents. The tension is not resolved by choosing one type; it is captured by recognizing the hybrid structure. The rising extractiveness (0.32 → 0.52) over the interval indicates that the extraction component is intensifying relative to coordination as the program matures — federal bureaucracy grows, contractor networks capture benefits, and coordination necessity declines. The true analytical issue is whether the constraint has an embedded sunset (scaffold) — will municipal autonomy and coordination capacity recover after federal program conclusion, or does dependency persist (piton)? The measurement trajectory (rising theater ratio) suggests the constraint is degrading from scaffold toward piton — federal oversight persists through bureaucratic inertia rather than functional necessity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    federal_blueprint_necessity,
    'Is federal blueprint enforcement a genuine coordination requirement or a cover for centralized resource control?',
    'Counterfactual analysis: cities that received Model Cities funding in early years but developed autonomous planning capacity afterward — do coordination outcomes degrade if federal blueprint enforcement is relaxed? Comparison with other federal-local coordination models that achieved outcomes with lower blueprint control.',
    'If necessary: constraint is rope with enforcement overhead. If control mechanism: constraint is snare masked as coordination. Classification shifts toward higher extraction if blueprints are pretextual.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(federal_blueprint_necessity, empirical, 'Whether federal blueprint enforcement is genuine coordination requirement').

omega_variable(
    displacement_compensation_adequacy,
    'Are relocation payments and replacement housing sufficient to offset forced displacement costs, or do they systematically undercompensate?',
    'Longitudinal economic tracking of displaced residents: income trajectories, housing cost burden, wealth accumulation compared to control groups in non-displaced neighborhoods. Assessment of replacement housing quality and location accessibility relative to original neighborhoods.',
    'If adequate compensation: displacement is high-cost coordination problem affecting minority of population. If systematic undercompensation: constraint is pure extraction masquerading as urban renewal — extractiveness rises toward 0.68+ (snare threshold).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(displacement_compensation_adequacy, empirical, 'Whether displacement compensation adequately offsets resident costs').

omega_variable(
    municipal_autonomy_recovery,
    'After federal Model Cities funding ends, do municipalities retain autonomous planning capacity or become dependent on federal direction?',
    'Institutional analysis of cities 5, 10, 20 years after Model Cities program conclusion. Assessment of whether cities continue federal-style coordinated planning or revert to local decision-making. Track funding source diversity — federal vs local revenue autonomy.',
    'If autonomous capacity develops: scaffold sunset logic is real. If dependency persists: constraint becomes institutional piton — federal direction becomes inertial rather than functional.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(municipal_autonomy_recovery, empirical, 'Whether municipal autonomy recovers after federal program conclusion').

omega_variable(
    non_designated_city_political_mobilization,
    'Do non-designated municipalities develop sufficient organized political pressure to access program benefits or compete for alternative federal funding?',
    'Political history of state and municipal advocacy coalitions 1969-1980. Track whether Model Cities generates political backlash that reshapes federal urban funding. Assess whether excluded cities successfully pressure Congress for program expansion or alternative funding mechanisms.',
    'If successful mobilization: non-designated cities escape victimhood (exit_options improve from trapped toward mobile). If quiescent: extraction persists as political inertia — victims lack agency to organize opposition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(non_designated_city_political_mobilization, empirical, 'Political mobilization of non-designated municipalities').

omega_variable(
    contractor_rent_seeking_intensity,
    'Do federal contracts under Model Cities exhibit cost-plus escalation and political allocation typical of extractive patronage systems?',
    'Audit analysis: comparison of Model Cities project costs against competitive-bid estimates for equivalent work. Assessment of contractor selection process — competitive vs political criteria. Track profit margins across contractor types and political connection levels.',
    'If rent-seeking is high: piton classification confirmed — federal oversight is theater masking contractor extraction. If procurement is disciplined: federal coordination is functional. Theater ratio interpretation changes.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(contractor_rent_seeking_intensity, empirical, 'Contractor rent-seeking intensity under federal oversight').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sotu_1969_johnson_model_cities_program, 1969, 1975).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(modelcities_theater_1969, sotu_1969_johnson_model_cities_program, theater_ratio, 0, 0.38).
narrative_ontology:measurement(modelcities_theater_1972, sotu_1969_johnson_model_cities_program, theater_ratio, 3, 0.51).
narrative_ontology:measurement(modelcities_theater_1975, sotu_1969_johnson_model_cities_program, theater_ratio, 6, 0.64).

% Extraction over time
narrative_ontology:measurement(modelcities_extractiveness_1969, sotu_1969_johnson_model_cities_program, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(modelcities_extractiveness_1972, sotu_1969_johnson_model_cities_program, base_extractiveness, 3, 0.42).
narrative_ontology:measurement(modelcities_extractiveness_1975, sotu_1969_johnson_model_cities_program, base_extractiveness, 6, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sotu_1969_johnson_model_cities_program, resource_allocation).
narrative_ontology:affects_constraint(sotu_1969_johnson_model_cities_program, urban_renewal_displacement_mechanism).
narrative_ontology:affects_constraint(sotu_1969_johnson_model_cities_program, federal_municipal_planning_authority).
narrative_ontology:affects_constraint(sotu_1969_johnson_model_cities_program, interstate_resource_distribution_equity).

% DUAL FORMULATION NOTE:
% Model Cities is the upstream constraint coordinating federal urban development policy. It structurally affects downstream constraints: (1) displacement_mechanism tracks specific relocation extraction from residents; (2) federal_municipal_authority tracks autonomy loss for city governments; (3) interstate_equity tracks relative deprivation for excluded municipalities. Each downstream constraint has different ε reflecting distinct extraction mechanisms — resource allocation (model cities itself), relocation coercion (displacement), authority asymmetry (planning), and political allocation (equity). All three are linked through the federal program structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
