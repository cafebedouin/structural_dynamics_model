% ============================================================================
% CONSTRAINT STORY: gp_fee_structure_asymmetry
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gp_fee_structure_asymmetry, []).

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
 *   constraint_id: gp_fee_structure_asymmetry
 *   human_readable: GP Fee Structure Asymmetry in Primary Healthcare
 *   domain: healthcare/economics/primary_care
 *
 * SUMMARY:
 *   The general practice fee structure in primary healthcare systems
 *   (particularly capitation-based or per-patient-contact models) creates a
 *   structural asymmetry between incentives for high-volume/low-complexity
 *   care and the actual needs of vulnerable populations requiring intensive,
 *   longitudinal engagement. This constraint exhibits classic tangled_rope
 *   mechanics: legitimate coordination function (enabling systematic payment
 *   and practice sustainability) is layered with extractive dynamics
 *   (incentivizing selection away from complex patients, reducing time for
 *   prevention, concentrating profits among high-volume operators). The
 *   constraint shows evidence of lifecycle drift — extractiveness has
 *   increased from 0.42 to 0.58 over the measurement interval as
 *   administrative complexity, data requirements, and practice consolidation
 *   have intensified the selection pressures. Theater ratio remains moderate
 *   (0.48) because the coordination function is genuine and observable
 *   (practices do coordinate around payment schedules), but it is declining
 *   relative to extraction mechanisms.
 *
 * KEY AGENTS:
 *   - Low-Income and Complex-Needs Patients: Primary victims (powerless/trapped) — bear full extraction cost through deprioritization, shorter consultations, reduced preventive engagement, and systematic selection by high-volume practices
 *   - Complex-Case General Practitioners: Secondary victims and moderate actors (moderate/constrained) — face professional identity conflict between patient care orientation and economic survival; constrained to rural/deprived areas by practice location
 *   - High-Volume Practice Operators: Primary beneficiaries (institutional/arbitrage) — capture efficiency gains and profit margins through patient volume and complexity selection; large practice networks and private equity consolidators with exit options
 *   - Health System Administrators: Secondary beneficiaries (institutional/arbitrage) — benefit from predictable per-patient funding streams and reduced complexity management overhead in aggregate planning
 *   - Policy Reform Coalitions: Organized advocates (organized/constrained) — medical associations, patient groups, health economists proposing alternative payment models (capitation, complexity adjustment); constrained by political economy of entrenched interests
 *   - Legacy Fee-for-Service Infrastructure: Institutional actor (institutional/arbitrage) — billing systems, practice management software, administrative protocols; maintains constraint through inertia rather than function
 *   - Analytical Observer: Civilizational analyst (analytical/analytical) — risks treating resource allocation trade-offs as natural law rather than policy choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gp_fee_structure_asymmetry, 0.58).
domain_priors:suppression_score(gp_fee_structure_asymmetry, 0.62).
domain_priors:theater_ratio(gp_fee_structure_asymmetry, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gp_fee_structure_asymmetry, extractiveness, 0.58).
narrative_ontology:constraint_metric(gp_fee_structure_asymmetry, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(gp_fee_structure_asymmetry, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gp_fee_structure_asymmetry, tangled_rope).
narrative_ontology:human_readable(gp_fee_structure_asymmetry, "GP Fee Structure Asymmetry in Primary Healthcare").
narrative_ontology:topic_domain(gp_fee_structure_asymmetry, "healthcare/economics/primary_care").

domain_priors:requires_active_enforcement(gp_fee_structure_asymmetry).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gp_fee_structure_asymmetry, high_patient_load_providers).
narrative_ontology:constraint_beneficiary(gp_fee_structure_asymmetry, health_system_administrators).
narrative_ontology:constraint_beneficiary(gp_fee_structure_asymmetry, pharmaceutical_incentive_recipients).
narrative_ontology:constraint_victim(gp_fee_structure_asymmetry, low_income_patients).
narrative_ontology:constraint_victim(gp_fee_structure_asymmetry, complex_case_providers).
narrative_ontology:constraint_victim(gp_fee_structure_asymmetry, preventive_care_emphasis).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LOW-INCOME PATIENTS (SNARE) — Trapped by geographic isolation (rural areas), lack of transportation, and economic dependency on publicly-funded primary care. Fee structures incentivize quick visits for high-patient-volume providers; complex psychosocial conditions requiring longer engagement are systematically deprioritized. Patients bear full extraction cost with no exit option.
constraint_indexing:constraint_classification(gp_fee_structure_asymmetry, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: COMPLEX-CASE PROVIDERS (TANGLED ROPE) — GPs serving high-needs populations (chronic disease management, mental health, substance abuse) face structural barriers: per-patient fees reward volume over complexity, yet these populations require longer consultations and coordination. Constrained by career location (often rural/deprived areas), student debt, and professional identity. Also benefit from genuine coordination function — the fee structure does enable primary care access at scale.
constraint_indexing:constraint_classification(gp_fee_structure_asymmetry, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: HIGH-VOLUME PRACTICE OPERATORS (ROPE) — Institutional actors (large practice networks, private equity-backed consolidators) experience the fee structure as pure coordination: payment-per-patient-contact enables resource planning and cashflow predictability. Net beneficiaries — they can select patient populations, optimize scheduling, and maximize throughput. Arbitrage options (shifting to private practice, reducing complexity burdens) available.
constraint_indexing:constraint_classification(gp_fee_structure_asymmetry, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: POLICY REFORM ADVOCATES (SCAFFOLD) — Organized coalitions (medical associations, patient advocates, health economists) identify the fee structure as a temporary incentive misalignment with a solvable sunset: capitation models, outcome-based payment, and complexity-adjusted funding mechanisms are viable alternatives. See the current system as a transient institutional arrangement, not a permanent feature. Sunset logic: capitation and complexity-adjusted fee pilots are already running in several health systems.
constraint_indexing:constraint_classification(gp_fee_structure_asymmetry, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: HISTORICAL FEE-FOR-SERVICE LOGIC (PITON) — The fee-for-service model persists largely through institutional inertia and data system lock-in: twenty years of billing infrastructure, practice management software, and administrative protocols are built on per-contact fees. The primary function (enabling reimbursement) is no longer the binding constraint — electronic payment and capitation are technically viable. The ritual persists because alternatives haven't fully replaced the legacy system.
constraint_indexing:constraint_classification(gp_fee_structure_asymmetry, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / ECONOMIC CONSTRAINT VIEW (MOUNTAIN) — From a universal analytical perspective, the fee asymmetry reflects an inescapable economic principle: finite resources must be allocated, and per-unit-service pricing creates inherent volume-vs-depth trade-offs. Some systems always prioritize throughput over complexity. This perspective risks naturalizing what is actually a policy choice — the constraint is not immutable, but perspectival.
constraint_indexing:constraint_classification(gp_fee_structure_asymmetry, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gp_fee_structure_asymmetry_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(gp_fee_structure_asymmetry, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(gp_fee_structure_asymmetry, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(gp_fee_structure_asymmetry, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(gp_fee_structure_asymmetry, TR),
    TR >= 0.70.

:- end_tests(gp_fee_structure_asymmetry_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The fee structure creates measurable asymmetry: per-patient-contact payment rewards throughput and complexity reduction, while complex-needs care (mental health integration, substance abuse, chronic disease management requiring longitudinal engagement) requires higher time investment per contact with lower payment-per-hour ratios. The extraction is systematic rather than predatory — it emerges from rational economic incentives rather than explicit coercion. The value reflects that the extraction is meaningful but not totalizing: some complex-case GPs remain in practice, some high-volume operators do provide reasonable care, and the system still achieves basic access in most areas. Suppression (0.62): Moderate-high. Significant barriers prevent exit: low-income patients are geographically isolated and economically dependent on publicly-funded primary care; complex-case GPs are constrained by professional identity and student debt; health system administrators face resource constraints. But suppression is not absolute — limited exit pathways exist (private practice for GPs, insurance for affluent patients, migration to other health systems). Theater ratio (0.48): Low-moderate. The fee structure is functionally transparent — practices and administrators clearly understand per-patient payment mechanics. But transparency masks extraction dynamics: the performative element is the claim that fee-for-service aligns incentives with health outcomes (it demonstrably does not for complex populations). The theater is increasing as practice consolidation and data systems make the volume/complexity trade-off more visible.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates perspectival divergence despite uniform agreement on base metrics. Low-income patients see snare (pure extraction with no exit). Complex-case GPs see tangled_rope (mixed coordination and extraction with limited exit). High-volume operators see rope (coordination mechanism enabling practice planning). Policy advocates see scaffold (temporary misalignment with a solvable sunset through capitation reform). The legacy fee-for-service system sees piton (performative adherence to historical model). The analytical observer risks seeing mountain (inevitable resource allocation trade-off). The gap reveals that identical structural mechanics produce divergent classifications because exit options and beneficiary status differ by perspective. The consensus that 'fee structure matters for outcomes' does not resolve disagreement about constraint type — beneficiaries and victims interpret the same metrics through different structural positions.
 *
 * DIRECTIONALITY LOGIC:
 *   The fee structure's directionality reveals how per-patient payment creates asymmetric power. High-volume operators can select patient populations (low-acuity, quick-resolution cases) and achieve high income per hour; complex-case GPs cannot select (population-based or geographically assigned) and achieve low income per hour. The fee structure does not coerce high-volume selection through explicit quotas; it incentivizes selection through profit-per-time-unit. This creates the tangled_rope signature: coordination function (enabling systematic payment across dispersed practices) is inseparable from asymmetric extraction (incentive structure concentrates rewards on high-volume, low-complexity practices). Beneficiaries (high-volume operators, administrators) experience the constraint as benign coordination; victims (low-income patients, complex-case GPs) experience it as extractive. The directionality pipeline treats this symmetry breakdown as structural rather than perspectival — d values are derived from beneficiary/victim status + exit options, which are objectively measurable structural facts.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy for this constraint is whether the fee structure is a coordination mechanism (Rope) or an extraction mechanism (Snare) or both (Tangled Rope). The resolution requires empirical data on health outcomes stratified by practice complexity burden and patient needs — if high-volume practices achieve equivalent or better outcomes for complex populations as complex-case practices, the structure is efficient coordination (and may appear as Rope). If high-volume practices achieve worse outcomes, the extraction is functional (Snare or Tangled Rope confirmed). Current evidence suggests outcomes diverge by complexity (worse outcomes for complex-needs populations in high-volume settings), supporting Tangled Rope classification. The coordination function (enabling payment at scale) is genuine and serves a real coordination problem (dispersed primary care practices need systematic funding). But this coordination function is layered with asymmetric extraction (high-volume selection, depressed complex-care income, reduced preventive investment). Mandatrophy is not resolved because the system cannot be decomposed into 'pure coordination' and 'pure extraction' stories — they are structural twins, not separable constraints.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    complexity_adjustment_feasibility,
    'Can case complexity be measured reliably enough to adjust fees without creating perverse incentives (diagnosis creep, defensive documentation)?',
    'Pilot data from capitation systems with complexity adjustment; analysis of diagnostic inflation before/after implementation; comparison of similar health systems with different adjustment methodologies',
    'If feasible: tangled_rope reclassifies toward rope for complex-case providers; extraction mechanism is solvable through policy design. If infeasible: complexity becomes structural alibi, and snare dynamics persist.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(complexity_adjustment_feasibility, empirical, 'Whether complexity adjustment can be implemented without gaming').

omega_variable(
    preventive_care_economic_return,
    'Do preventive services in primary care (mental health screening, substance abuse intervention, chronic disease monitoring) produce measurable economic returns within the timeframe of typical GP contracts (2-5 years)?',
    'Long-term cost analysis of populations receiving integrated preventive care vs. standard care; hospital admission reduction tracking; emergency department utilization comparison; disability-adjusted life year (DALY) costing',
    'If returns are rapid: fee structure can be justified as efficient (shifting from snare to rope classification from low-income patient perspective). If returns are delayed: current fee structure is extraction hiding behind economic rationality.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(preventive_care_economic_return, empirical, 'Whether preventive primary care has measurable near-term economic return').

omega_variable(
    rural_viability_threshold,
    'What minimum fee level sustains GP practice in rural/deprived areas, and does current fee structure meet this threshold without supplementary incentives?',
    'Cost accounting of rural vs urban practices; correlation of GP density with baseline fee levels across health systems; analysis of rural practice closures and provider recruitment failure rates',
    'If current fees are below viability threshold: system requires extractive subsidization from other parts of healthcare (tangled_rope confirmed). If fees exceed threshold: system is paying for inefficiency (piton confirmed).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(rural_viability_threshold, empirical, 'Whether current fee structure meets rural practice viability threshold').

omega_variable(
    patient_outcome_divergence,
    'Do high-volume practices systematically achieve worse health outcomes (morbidity, mortality, quality of life) for complex-needs populations compared to low-volume complex-case practices?',
    'Outcome tracking: disease control rates, hospital readmission rates, mental health functioning, quality of life scores; stratification by practice volume and patient complexity; risk adjustment analysis',
    'If outcomes diverge: fee structure is directly extractive (snare from patient perspective). If outcomes don''t diverge: extraction is not translating to health harm (snare mechanism is structural but not functional).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(patient_outcome_divergence, empirical, 'Whether high-volume practices achieve worse outcomes for complex-needs patients').

omega_variable(
    political_economy_of_reform,
    'Why does healthcare policy reform toward capitation/complexity-adjusted fees stall despite consistent advocacy from medical associations, patient groups, and health economists?',
    'Historical analysis of failed reform attempts; stakeholder interviews; cost-benefit analysis of transition to alternative payment models; political economy of provider organization interests',
    'If stalling is technical (complexity measurement, implementation cost): scaffold perspective is valid — sunset is achievable with sufficient policy attention. If stalling is political (powerful beneficiaries blocking reform): system has captured its would-be reformers (snare/tangled_rope extraction mechanisms are stronger than stated).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(political_economy_of_reform, conceptual, 'Political barriers to reform toward capitation models').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gp_fee_structure_asymmetry, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gpfee_tr_t0, gp_fee_structure_asymmetry, theater_ratio, 0, 0.35).
narrative_ontology:measurement(gpfee_tr_t5, gp_fee_structure_asymmetry, theater_ratio, 5, 0.42).
narrative_ontology:measurement(gpfee_tr_t10, gp_fee_structure_asymmetry, theater_ratio, 10, 0.48).

% Extraction over time
narrative_ontology:measurement(gpfee_be_t0, gp_fee_structure_asymmetry, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(gpfee_be_t5, gp_fee_structure_asymmetry, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(gpfee_be_t10, gp_fee_structure_asymmetry, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gp_fee_structure_asymmetry, resource_allocation).
narrative_ontology:boltzmann_floor_override(gp_fee_structure_asymmetry, 0.18).
narrative_ontology:affects_constraint(gp_fee_structure_asymmetry, mental_health_primary_care_integration).
narrative_ontology:affects_constraint(gp_fee_structure_asymmetry, preventive_care_access_disparities).
narrative_ontology:affects_constraint(gp_fee_structure_asymmetry, rural_gp_recruitment_failure).

% DUAL FORMULATION NOTE:
% The GP fee structure asymmetry is upstream of downstream constraints in mental health integration, preventive care disparities, and rural recruitment. Each downstream constraint has its own extractiveness value reflecting domain-specific dynamics, but all three are partially driven by the fee structure's extraction mechanism. Capitation reform would directly affect all three downstream constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(gp_fee_structure_asymmetry, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
