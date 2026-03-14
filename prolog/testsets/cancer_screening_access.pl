% ============================================================================
% CONSTRAINT STORY: cancer_screening_access
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_cancer_screening_access, []).

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
 *   constraint_id: cancer_screening_access
 *   human_readable: Cancer Screening Access Asymmetry
 *   domain: healthcare/public_health
 *
 * SUMMARY:
 *   Cancer screening access is a structurally complex constraint that
 *   exhibits genuine coordination benefits alongside systematic asymmetric
 *   extraction. The system must coordinate diverse actors — clinicians,
 *   laboratories, insurers, patients, public health authorities — to deliver
 *   screening at population scale. Simultaneously, the institutional
 *   arrangement (tied to insurance status, geographic location, income)
 *   systematically denies screening to the most vulnerable populations while
 *   capturing benefits for healthcare providers and pharmaceutical companies.
 *   This creates a tangled rope: legitimate coordination function married to
 *   extractive access asymmetry. The constraint's evolution shows increasing
 *   extraction (base extractiveness rising from 0.42 to 0.58 over the
 *   interval) as administrative complexity (theater) grows while access
 *   barriers remain static. The challenge for classification is
 *   distinguishing genuine coordination difficulty from weaponized complexity
 *   that justifies access denial.
 *
 * KEY AGENTS:
 *   - Low-Income Uninsured Populations: Primary victims (powerless/trapped) — face absolute barriers to screening; bear cost of delayed diagnosis and preventable mortality
 *   - Rural Populations: Secondary victims (powerless/trapped) — geographic isolation compounds income barriers; generational perpetuation of constraint
 *   - Insured Middle-Income Patients: Mixed (moderate/constrained) — can access screening but face high copays and fragmented coordination
 *   - Large Healthcare Systems: Beneficiary institutions (institutional/arbitrage) — profit from screening volume, can arbitrage away from low-margin populations
 *   - Pharmaceutical Manufacturers: Pure beneficiaries (institutional/arbitrage) — screening tests are revenue streams; benefit from access asymmetry and price differentiation
 *   - Public Health Coalition: Organized reformers (organized/constrained) — building alternative pathways (community health workers, mobile units, universal funding advocacy) toward sunset
 *   - Guideline-Setting Authorities: Institutional actors (institutional/arbitrage) — maintain fragmented recommendation ecosystem; benefit from complexity; theater persists through inertia
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — sees both genuine coordination function and contingent extraction mechanism
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(cancer_screening_access, 0.58).
domain_priors:suppression_score(cancer_screening_access, 0.62).
domain_priors:theater_ratio(cancer_screening_access, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(cancer_screening_access, extractiveness, 0.58).
narrative_ontology:constraint_metric(cancer_screening_access, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(cancer_screening_access, theater_ratio, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(cancer_screening_access, tangled_rope).
narrative_ontology:human_readable(cancer_screening_access, "Cancer Screening Access Asymmetry").
narrative_ontology:topic_domain(cancer_screening_access, "healthcare/public_health").

domain_priors:requires_active_enforcement(cancer_screening_access).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(cancer_screening_access, pharmaceutical_manufacturers).
narrative_ontology:constraint_beneficiary(cancer_screening_access, healthcare_administrators).
narrative_ontology:constraint_beneficiary(cancer_screening_access, private_screening_providers).
narrative_ontology:constraint_victim(cancer_screening_access, low_income_populations).
narrative_ontology:constraint_victim(cancer_screening_access, rural_populations).
narrative_ontology:constraint_victim(cancer_screening_access, uninsured_populations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LOW-INCOME UNINSURED PATIENT (SNARE) — Structurally unable to exit the constraint. Cannot afford screening costs (~$500-2000 for comprehensive workup), lack insurance coverage, face transportation and time barriers. Early detection requires screening access; absence of screening means later-stage diagnosis with worse outcomes and higher ultimate costs. The victim is fully trapped — extraction takes the form of delayed treatment, preventable deaths, and higher medical debt when cancer is finally detected.
constraint_indexing:constraint_classification(cancer_screening_access, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: RURAL POPULATION (SNARE) — Geographic isolation creates structural trap independent of income. Nearest screening facility may be 50+ miles away; transportation costs and lost work time compound the economic barrier. At generational horizon, the constraint perpetuates: lack of demand doesn't justify local infrastructure investment, so individuals continue trapped by geography. Suppression is maximal — no realistic exit option exists.
constraint_indexing:constraint_classification(cancer_screening_access, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 3: INSURED MIDDLE-INCOME PATIENT (TANGLED ROPE) — Has insurance and can afford screening, but faces coordination constraints: scheduling delays, multiple referrals, fragmented provider networks. Benefits from genuine coordination function (screening programs integrate multiple specialists), but also bears asymmetric extraction: high copays, coverage denials, limited provider choices. High enough exit cost (losing insurance means entering trapped state) that exit is constrained rather than mobile, but not insurmountable.
constraint_indexing:constraint_classification(cancer_screening_access, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: LARGE HEALTHCARE SYSTEM ADMINISTRATOR (ROPE) — Experiences screening access primarily as a coordination mechanism: organizing workflows, standardizing protocols, integrating oncology and primary care. Benefits from high-volume screening programs (revenue stability, predictable patient flow). Can arbitrage: shift screening focus to profitable demographics, consolidate services, reduce access in low-margin populations. Sees the constraint as manageable coordination that generates institutional benefit.
constraint_indexing:constraint_classification(cancer_screening_access, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: PHARMACEUTICAL MANUFACTURER (ROPE) — Pure beneficiary. Screening tests and diagnostics are revenue streams. Benefits from access asymmetry: insured populations drive demand and profit; uninsured populations represent untapped market with price sensitivity. Can arbitrage: develop expensive proprietary tests, lobby for coverage expansion in high-income markets, maintain price structures that exclude low-income access. Sees screening access policy primarily as a coordination mechanism for capturing market share.
constraint_indexing:constraint_classification(cancer_screening_access, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: PUBLIC HEALTH COALITION (SCAFFOLD) — Organized agents (American Cancer Society, health departments, advocacy nonprofits) view screening access as a temporary coordination failure with a sunset trajectory. Community health worker programs, mobile screening units, and policy reform (insurance mandates, public funding) are building alternative pathways. See the current extraction mechanism as degrading — as universal screening programs mature and prevention-focused reimbursement models scale, the access asymmetry becomes untenable. Constrained exit (political barriers to implementation) but genuine path to sunset.
constraint_indexing:constraint_classification(cancer_screening_access, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: FRAGMENTED SCREENING GUIDELINES SYSTEM (PITON) — Multiple professional societies publish slightly different screening recommendations (USPSTF, ACS, AMA), creating performative complexity. Healthcare systems maintain expensive guideline-adherence infrastructure despite low additional medical benefit from guideline variation. Theater ratio reflects the ritual of coordinating across guidelines — significant administrative burden with marginal impact on outcomes. Theater has increased as guideline proliferation outpaced evidence differentiation. Maintained through institutional inertia rather than functional necessity.
constraint_indexing:constraint_classification(cancer_screening_access, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER (TANGLED ROPE) — From civilizational scope, screening access reveals genuine coordination function (integrating population health, disease surveillance, clinical services) alongside systematic extraction (capturing differential health outcomes by socioeconomic status). Base extractiveness reflects both: the system solves real coordination problems AND reproduces health inequities. Unlike the mountain-claiming analysis (that health disparities are inevitable), the structural data shows extraction is contingent on policy choices: universal screening programs demonstrate that access can be decoupled from ability to pay.
constraint_indexing:constraint_classification(cancer_screening_access, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cancer_screening_access_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(cancer_screening_access, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(cancer_screening_access, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(cancer_screening_access, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(cancer_screening_access, TR),
    TR >= 0.70.

:- end_tests(cancer_screening_access_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The constraint extracts through access asymmetry (uninsured and rural populations face complete or near-complete exclusion) and through forced compliance with expensive coordinated systems (insured patients must navigate fragmented provider networks and pay significant out-of-pocket costs). The extractiveness value reflects that a significant health population bears substantial cost with limited benefit capture, while providers and manufacturers capture market-based benefit. The value is not maximum because public health campaigns and some safety-net screening programs partially compensate for market failures. Suppression (0.62): High. Multiple structural barriers suppress exit: cost barriers (screening is unaffordable for uninsured), geographic barriers (rural populations lack local infrastructure), time barriers (working populations cannot take off work for appointments), and informational barriers (screening awareness is lower in low-income communities). However, suppression is not absolute — some populations do achieve screening through safety-net systems. Theater ratio (0.45): Moderate-low. The functional delivery of screening involves genuine care coordination (multiple clinicians, integration with electronic health records, laboratory quality control). However, administrative overhead has grown significantly — guideline management, payer verification, prior authorization, and documentation requirements constitute performative burden. The theater ratio reflects that coordination has real functional content but is encumbered with administrative complexity not clearly improving outcomes.
 *
 * PERSPECTIVAL GAP:
 *   The largest gap appears between trapped victims (snare) and institutional beneficiaries (rope). An uninsured patient sees a system that denies them health benefit while enriching corporations. A healthcare administrator sees a well-coordinated system that they operate successfully. Both are accurate perspectives on the same structure. The gap reveals that 'coordination function' and 'extraction mechanism' are not mutually exclusive — the system coordinates effectively while extracting asymmetrically. The scaffold perspective (public health coalition) is critical because it shows the constraint is not inevitable: community health worker models, mobile screening units, and policy shifts demonstrate that screening can be decoupled from ability-to-pay. This means the snare classification is contingent on policy choice, not structural necessity.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by structural position relative to screening access flows. Uninsured and rural populations (trapped, powerless) have d approaching 1.0 — they are pure targets, bearing all suppression cost with no mitigation option. Insured middle-income patients (constrained, moderate) have d around 0.65 — they can access screening but face significant out-of-pocket cost and network constraints. Healthcare administrators and pharmaceutical manufacturers (arbitrage, institutional) have d around 0.10 — they are beneficiaries capturing revenue and market position from screening programs. The analytical observer (analytical exit, civilizational scope) has d around 0.72 — positioned to see the full structure and recognize that the extraction mechanism is contingent on policy choices rather than inevitable. The public health coalition (constrained, organized) has lower d than trapped populations because they have organizing capacity and perceivable policy levers, even if political barriers are high.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY CANDIDATE: This constraint risks false naturalization as a market-determined fact ('some people can afford healthcare, some cannot — that's just how markets work'). The mountain-claiming analysis would assert that health inequality is inherent to resource scarcity. However, the structural data contradicts this: (1) public health systems with universal screening demonstrate equal access is achievable, (2) the bottleneck is not technical (we know how to screen) but institutional (funding and access policy), (3) the extraction mechanism is contingent on insurance-tied access, not on scarcity itself. Therefore, this is NOT a mountain. The mandatrophy resolution: The constraint is tangled_rope because it contains both genuine coordination (screening delivery at scale requires integrated infrastructure) and genuine extraction (access is systematically denied based on ability to pay). The coordination part is real; the extraction part is policy-contingent. The scaffold sunset is realistic if policy shifts (universal screening funding, community health worker integration, public insurance expansion) occur. No mandatrophy paradox at tangled_rope — both the rope and snare classifications are legitimate from their respective positions.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    screening_harm_incidence,
    'What proportion of screening benefits is offset by false positives, overdiagnosis, and unnecessary interventions in low-resource populations?',
    'Longitudinal cohort studies comparing screened vs unscreened low-income populations; analysis of false positive cascade costs and harms; quality-adjusted life year calculations including overtreatment harm',
    'If harms are substantial: screening access expansion without infrastructure support constitutes harm-generating extraction. If harms are minimal: access expansion is pure benefit and the constraint is primarily about resource allocation inequity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(screening_harm_incidence, empirical, 'Proportion of screening harm vs benefit in low-resource populations').

omega_variable(
    treatment_access_bottleneck,
    'Is screening access the binding constraint on cancer mortality reduction, or is treatment access the actual bottleneck?',
    'Comparative analysis: populations with screening access but limited treatment access vs populations with both or neither; outcome data showing mortality reduction from screening alone vs screening+treatment',
    'If treatment is the bottleneck: expanding screening without treatment access may increase psychological harm and financial burden without mortality benefit, making the constraint primarily extractive. If screening is binding: expanding access generates measurable mortality reduction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(treatment_access_bottleneck, empirical, 'Whether screening access or treatment access is the binding mortality constraint').

omega_variable(
    preventive_care_coordination_necessity,
    'How much of the coordination function attributed to screening programs is genuine health system integration vs performative ''care coordination'' theater?',
    'Outcome analysis of integrated vs fragmented screening delivery; cost-effectiveness studies; clinician and patient surveys on perceived coordination value',
    'If coordination is genuine: the constraint is tangled_rope with real coordination benefit alongside extraction. If largely theater: the constraint approaches snare (pure extraction with coordination as cover story).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(preventive_care_coordination_necessity, empirical, 'Genuine vs performative value of screening program coordination').

omega_variable(
    public_funding_sufficiency,
    'Could universal public screening funding eliminate the access-based extraction, or is the constraint structural to capitalist healthcare economics?',
    'Policy analysis of countries with universal screening funding; cost modeling for US universal screening; analysis of whether eliminated access barriers would shift extraction to other dimensions (wait times, quality variation, etc.)',
    'If universal funding resolves: constraint is policy-contingent and scaffold sunset is realistic. If extraction persists in different form: constraint is deeper structural and mandatrophy requires reconceptualization.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(public_funding_sufficiency, conceptual, 'Whether universal funding can resolve the access extraction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(cancer_screening_access, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(csa_tr_t0, cancer_screening_access, theater_ratio, 0, 0.35).
narrative_ontology:measurement(csa_tr_t5, cancer_screening_access, theater_ratio, 5, 0.4).
narrative_ontology:measurement(csa_tr_t10, cancer_screening_access, theater_ratio, 10, 0.45).

% Extraction over time
narrative_ontology:measurement(csa_be_t0, cancer_screening_access, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(csa_be_t5, cancer_screening_access, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(csa_be_t10, cancer_screening_access, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(cancer_screening_access, resource_allocation).
narrative_ontology:boltzmann_floor_override(cancer_screening_access, 0.18).
narrative_ontology:affects_constraint(cancer_screening_access, health_insurance_access).
narrative_ontology:affects_constraint(cancer_screening_access, rural_healthcare_infrastructure).
narrative_ontology:affects_constraint(cancer_screening_access, pharmaceutical_pricing_power).

% DUAL FORMULATION NOTE:
% Cancer screening access is downstream of healthcare system organization and pharmaceutical market structure. The upstream constraints (insurance access, rural infrastructure, drug pricing) determine the conditions under which screening operates. Screening access is both a coordination problem (integrating multiple care elements) and an extraction mechanism (the system profits from access asymmetry). Decomposition opportunity: screening access for different cancer types (breast, colorectal, prostate) may have different ε values and constraint profiles depending on test cost, infrastructure requirements, and treatment complexity.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(cancer_screening_access, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
