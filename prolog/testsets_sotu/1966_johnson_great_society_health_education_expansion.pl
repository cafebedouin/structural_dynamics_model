% ============================================================================
% CONSTRAINT STORY: 1966_johnson_great_society_health_education_expansion
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_1966_johnson_great_society_health_education_expansion, []).

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
 *   constraint_id: 1966_johnson_great_society_health_education_expansion
 *   human_readable: Federal Health and Education Program Expansion via Great Society (1965-1975)
 *   domain: social_policy/education/public_health
 *
 * SUMMARY:
 *   President Lyndon Johnson's Great Society initiatives (1965-1969,
 *   sustained through the 1970s) represent a pivotal institutional expansion:
 *   the federal government assumes primary responsibility for redistributing
 *   resources to vulnerable populations through health and education programs
 *   (Medicare, Medicaid, Title I education funding). This constraint exhibits
 *   the defining feature of Tangled Rope: genuine coordination function
 *   (scaling education and healthcare delivery across 50 states requires
 *   federal standardization and funding mechanisms) combined with asymmetric
 *   extraction (federal bureaucracy creates compliance overhead; federal
 *   taxation is mandatory; state autonomy is reduced). The constraint's
 *   perspectival structure reveals the core tension of the Great Society:
 *   presented as moral coordination (ensuring vulnerable populations access
 *   basic services), experienced by some as coercive redistribution
 *   (mandatory taxation), by others as displacement (private welfare
 *   institutions), and by states as loss of autonomy. The theater ratio's
 *   rise (0.28 → 0.42 over the interval) reflects increasing
 *   bureaucratization: as programs mature, administrative processing and
 *   compliance documentation grow faster than service delivery growth,
 *   suggesting Goodhart drift — the programs' original coordination function
 *   (ensuring access) is increasingly obscured by theater (forms, reporting
 *   requirements, eligibility verification).
 *
 * KEY AGENTS:
 *   - Low-Income Families and Students: Primary beneficiary (powerless/constrained) — gain access to education, healthcare, nutrition assistance; net benefit despite constrained structural position
 *   - State Governments: Secondary actor (organized/constrained) — lose autonomy in welfare policy; gain access to federal resources and coordination benefits; experience extraction through match requirements and reduced discretion
 *   - Federal Taxpayer Base: Primary victim (powerless/trapped) — mandatory extraction without direct coordination benefit; structured legal suppression; no exit mechanism
 *   - Federal Administrative Apparatus: Primary beneficiary (powerful/mobile) — budget growth, institutional prestige, expanded jurisdiction; experiences genuine coordination function (scaling delivery across states)
 *   - Private Welfare Institutions: Displaced actor (institutional/arbitrage) — lose primary function as federal programs provide basic services; maintain reduced roles through institutional inertia
 *   - Civil Rights Coalition: Organized agents (institutional/arbitrage) — leverage federal enforcement to override state segregation barriers; experience programs as temporary political tools with sunset logic
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent institutional design as inherent to industrial society
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(1966_johnson_great_society_health_education_expansion, 0.35).
domain_priors:suppression_score(1966_johnson_great_society_health_education_expansion, 0.28).
domain_priors:theater_ratio(1966_johnson_great_society_health_education_expansion, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(1966_johnson_great_society_health_education_expansion, extractiveness, 0.35).
narrative_ontology:constraint_metric(1966_johnson_great_society_health_education_expansion, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(1966_johnson_great_society_health_education_expansion, theater_ratio, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(1966_johnson_great_society_health_education_expansion, tangled_rope).
narrative_ontology:human_readable(1966_johnson_great_society_health_education_expansion, "Federal Health and Education Program Expansion via Great Society (1965-1975)").
narrative_ontology:topic_domain(1966_johnson_great_society_health_education_expansion, "social_policy/education/public_health").

domain_priors:requires_active_enforcement(1966_johnson_great_society_health_education_expansion).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(1966_johnson_great_society_health_education_expansion, low_income_families).
narrative_ontology:constraint_beneficiary(1966_johnson_great_society_health_education_expansion, students_in_poverty).
narrative_ontology:constraint_beneficiary(1966_johnson_great_society_health_education_expansion, healthcare_service_providers).
narrative_ontology:constraint_beneficiary(1966_johnson_great_society_health_education_expansion, federal_administrative_apparatus).
narrative_ontology:constraint_victim(1966_johnson_great_society_health_education_expansion, taxpayer_base).
narrative_ontology:constraint_victim(1966_johnson_great_society_health_education_expansion, state_fiscal_autonomy).
narrative_ontology:constraint_victim(1966_johnson_great_society_health_education_expansion, private_welfare_institutions).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LOW-INCOME FAMILY (ROPE) — Structurally mobile (could relocate, children could enter workforce) but constrained by economic dependency. The federal programs provide direct coordination benefit: food assistance, healthcare, educational access. Suppression is real (poverty itself is the constraint) but the coordination function is genuine — programs solve the collective problem of vulnerable populations accessing basic services. Family experiences net benefit; extraction is minimal relative to gained access.
constraint_indexing:constraint_classification(1966_johnson_great_society_health_education_expansion, rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 2: STATE GOVERNMENT (TANGLED ROPE) — Previously autonomous in welfare policy; now constrained by federal mandate and budget match requirements. Experiences coordination benefit (federal standards enable interstate coordination, prevent race-to-the-bottom in education quality) alongside asymmetric extraction (federal rules reduce state discretion, federal matching requirements strain state budgets). State-level enforcement of federal curriculum and healthcare standards creates dual accountability structure: state bureaucrats report to both state voters and federal administrators.
constraint_indexing:constraint_classification(1966_johnson_great_society_health_education_expansion, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: FISCAL COMMONS / TAXPAYER BASE (SNARE) — Organized extraction without coordination benefit. Federal taxation to fund social programs is mandatory; taxpayers cannot exit the system. Extraction is structured, not negotiated. No coordination benefit accrues to individual taxpayers (unlike state-level redistribution where community members vote directly). Maximum suppression: tax compliance is legally enforced; refusal has criminal consequences. The taxpayer base bears costs without meaningful voice in allocation decisions at federal scale.
constraint_indexing:constraint_classification(1966_johnson_great_society_health_education_expansion, snare,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 4: FEDERAL ADMINISTRATIVE APPARATUS (TANGLED ROPE) — Benefits from program expansion (budget growth, institutional prestige, expanded jurisdiction). Experiences genuine coordination problem: scaling healthcare and education delivery across 50 states requires standardized protocols, funding mechanisms, enforcement capacity. Federal agencies coordinate this at scale. But also extracts: bureaucratic requirements create processing overhead; federal compliance regimes generate rents for consulting firms and administrative contractors. Agency experiences this as coordination; beneficiary populations experience it as bureaucracy.
constraint_indexing:constraint_classification(1966_johnson_great_society_health_education_expansion, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: PRIVATE WELFARE INSTITUTIONS (PITON) — Churches, voluntary organizations, and private charities previously provided education and healthcare to poor populations. Federal programs displace this role, reducing the institutional function of private welfare. These institutions persist in reduced capacity (pastoral care, supplementary services) but lose their primary function (basic service delivery). Theater increases as these institutions emphasize cultural/spiritual role rather than material provision. Extracted from their historical role by federal displacement, yet maintained through inertia and community attachment.
constraint_indexing:constraint_classification(1966_johnson_great_society_health_education_expansion, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 6: CIVIL RIGHTS COALITION (SCAFFOLD) — Benefits from federal enforcement mechanisms that (1) redirect resources to historically excluded populations and (2) override state-level segregation barriers. Sees programs as temporary leverage points — federal enforcement creates political space for local organizing and demands escalation. Coalition members experience high agency; suppression is low (enforcement can be escalated, programs can be expanded). Sunset clause: as institutional integration and federal civil rights law mature, the need for program-based redistribution as a tool for overcoming segregation decreases.
constraint_indexing:constraint_classification(1966_johnson_great_society_health_education_expansion, scaffold,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From civilizational scope, the Great Society programs appear to solve an immutable problem: pre-modern societies distribute survival resources through kinship and community; industrial societies require institutional mechanisms. Poverty and health inequality are structural features of market economies; federal redistribution is an inevitable institutional response. This perspective risks naturalizing what is a contingent political choice — treating the federal coordination mechanism as inherent to industrial society rather than as a specific institutional design. The engine's false summit detector will identify beneficiary presence, revealing the naturalization.
constraint_indexing:constraint_classification(1966_johnson_great_society_health_education_expansion, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(1966_johnson_great_society_health_education_expansion_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(1966_johnson_great_society_health_education_expansion, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(1966_johnson_great_society_health_education_expansion, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(1966_johnson_great_society_health_education_expansion, TR),
    TR >= 0.70.

:- end_tests(1966_johnson_great_society_health_education_expansion_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.35): Moderate. The constraint exhibits both genuine coordination (standardization enables interstate cooperation, prevents destructive competition, scales delivery) and extraction (federal bureaucracy adds overhead; mandatory taxation; state autonomy reduced). The 0.35 value reflects that the coordination function is substantial but not dominant. Rising from 0.18 (initial programs, high coordination enthusiasm) to 0.35 (mature programs, bureaucracy entrenched) suggests Goodhart drift — original coordination intent increasingly displaced by administrative theater. Suppression (0.28): Moderate. Legal suppression of taxpayer exit is complete (criminal penalties for tax evasion), but program beneficiaries experience low suppression (programs are visible, tangible benefits, not coercive). State governments experience moderate suppression (federal mandates constrain options but don't eliminate them). Overall: legal suppression is real but not catastrophic; beneficiary populations experience programs as enabling rather than suppressive. Theater ratio (0.42): Moderate-rising. Early programs are high-function, low-theater (food assistance, school funding). As bureaucratization increases, theater grows: eligibility verification, compliance documentation, outcome measurement become increasingly performative (e.g., programs emphasize metrics that satisfy federal reporting requirements rather than optimizing service delivery). The trajectory suggests Piton drift — if theater continues rising, perspective classification will eventually shift from Tangled Rope toward Piton (degraded institutional function maintained through inertia).
 *
 * PERSPECTIVAL GAP:
 *   This constraint's perspectival structure reveals the core conflict between redistributive institutions and federalist structure. The low-income family sees a coordinating mechanism (Rope); the taxpayer sees an extraction mechanism (Snare); the state sees both (Tangled Rope). The gap is not perceptual error but structural reality: the same institutional apparatus is genuinely both coordinating (for those who gain access) and extractive (for those who pay for others' access). The framework dissolves the false debate ('is the Great Society good or bad?') by classifying what it is from each structural position. To powerless families, it is good (coordinate access solution). To taxpayers, it is coercive (mandatory extraction). To states, it is mixed (gains and losses). The mandatrophy resolution is: all perspectives are correct from their structural positions. The constraint is a Tangled Rope from the institutional design perspective because it genuinely coordinates while genuinely extracting.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from beneficiary/victim status combined with exit options. Low-income families are beneficiaries with constrained exit: d ≈ 0.30 (derive from beneficiary status + constrained exit). Taxpayers are victims with trapped exit (legal suppression): d ≈ 0.92 (maximum extraction). States are mixed (both beneficiary and victim): d ≈ 0.55 (federal matching funds provide benefit; federal mandates extract autonomy). Federal administrators are beneficiaries with mobile/arbitrage exit: d ≈ 0.10 (low extraction, high institutional benefit). Private institutions are victims with arbitrage exit (can relocate focus to other activities): d ≈ 0.65 (moderate extraction via displacement). Civil rights coalition are beneficiaries with constrained-to-organized exit (increasing power): d ≈ 0.35 (moderate benefit, increasing agency). The chi values derive from these d values via the sigmoid f(d), producing the perspectival classifications: beneficiaries with low d → Rope or Scaffold; victims with high d → Snare; mixed positions with mid-range d → Tangled Rope.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that the Great Society combines genuine coordination function with asymmetric extraction, which is exactly the Tangled Rope definition. The coordination function is real: scaling education and healthcare access across 50 states requires federal standardization, funding, and enforcement. No state alone can solve the coordination problem of preventing a race-to-the-bottom in education quality or ensuring access for mobile populations. The asymmetric extraction is also real: federal bureaucracy creates processing overhead; federal tax structure is mandatory with legal suppression of exit; some actors (private welfare institutions, state governments) bear costs without equivalent benefit. The Tangled Rope classification recognizes both: this is not pure redistribution (which could be Rope) and not pure extraction (which would be Snare). It is hybrid — the coordination function is genuinely necessary, and the extraction is genuinely asymmetric. The theater ratio's rise suggests drift toward Piton (where coordination function atrophies but bureaucracy persists), but at the 1965-1975 interval, the Tangled Rope holds. The false summit perspective (Mountain) is a naturalization risk: treating federal coordination as inherent to industrial society rather than as a contingent institutional design with real distributive consequences.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_vs_extraction_boundary,
    'What proportion of the programs'' cost is genuine coordination overhead (standardization, enforcement infrastructure) vs. redistributive extraction (transfer from high-income to low-income agents)?',
    'Decompose program budgets into administrative cost vs. direct service delivery. Compare federal unit costs to state-level or private-provider unit costs for equivalent services. Measure coordination benefits (interstate standardization, prevention of race-to-bottom) directly.',
    'High coordination cost (>40% of budget): reclassify toward Rope from multiple perspectives. Low coordination cost (<20%): reclassify toward Snare/Tangled Rope — emphasizes redistribution over coordination.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(coordination_vs_extraction_boundary, empirical, 'Proportion of cost that is genuine coordination overhead vs. redistribution').

omega_variable(
    state_autonomy_externality,
    'Does federal standardization of education and health prevent destructive interstate competition (positive coordination), or does it displace legitimate state-level policy experimentation (negative extraction from state autonomy)?',
    'Counterfactual analysis: what would education outcomes look like under a system of pure state competition without federal baseline standards? Measure correlation between federal standardization and convergence of education quality, and correlation between state freedom and outcome divergence.',
    'If prevention of race-to-bottom is substantial: coordination frame strengthens (Rope/Tangled Rope). If state innovation is suppressed: extraction frame strengthens (Snare perspective on states).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_autonomy_externality, conceptual, 'Whether federal standardization prevents destructive competition or suppresses state experimentation').

omega_variable(
    civil_rights_enforcement_coupling,
    'Are the health and education programs primarily mechanisms for resource redistribution to the poor, or are they primarily mechanisms for enforcing civil rights integration against segregationist state resistance?',
    'Temporal analysis: compare program expansion rates in segregationist vs. non-segregationist states. Analyze federal enforcement rhetoric and conditional funding requirements. Measure what fraction of program expansion correlates with civil rights enforcement vs. poverty reduction goals.',
    'If primarily civil rights enforcement: scaffold perspective is correct; programs have sunset logic (enforcement obsolete as segregation is defeated). If primarily redistribution: programs are indefinite (poverty is persistent); sunset logic fails.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(civil_rights_enforcement_coupling, empirical, 'Whether programs are primarily about redistribution or civil rights enforcement').

omega_variable(
    tax_compliance_voluntary_exit,
    'Do taxpayers in the Snare perspective have realistic exit options (relocation, tax avoidance, economic restructuring) that are suppressed by law, or is suppression complete at a structural level?',
    'Economic mobility analysis: measure percentage of high-income earners who relocate to avoid progressive taxation, measure tax avoidance strategies available, measure enforcement intensity across income levels. Determine if exit suppression is legal (criminal penalty for tax evasion) or structural (no realistic alternative revenue source).',
    'If realistic exit options exist but are suppressed by law: Snare classification holds (legal suppression). If exit is structurally impossible: reclassify toward Mountain (natural law of fiscal aggregation) or Rope (coordination benefit of stable revenue).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tax_compliance_voluntary_exit, empirical, 'Whether taxpayers face legal or structural suppression of exit options').

omega_variable(
    private_welfare_displacement_mechanism,
    'Do federal programs displace private welfare institutions through direct crowding-out (government services replace private services at lower cost), or through reallocation of donor interest (charitable giving shifts from private welfare to federal programs)?',
    'Time series analysis of private charitable giving, private institution funding, and program participation. Measure whether private institutions shrink due to loss of client demand (crowding-out) or loss of donor support (reallocation).',
    'If crowding-out: Piton classification is correct (institutions lose function but persist through inertia). If reallocation: institutions actively decline (stronger Snare perspective on private institutions).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(private_welfare_displacement_mechanism, empirical, 'Whether private welfare displacement is via crowding-out or donor reallocation').

omega_variable(
    false_summit_institutional_naturalization,
    'Is federal coordination of health and education inherent to industrial society (Mountain), or is it a contingent institutional design that benefits specific actors?',
    'Comparative analysis: countries at similar development levels with different institutional designs (market-based, decentralized, private-provision dominant). Measure whether outcomes (health, education) differ substantially or converge despite institutional diversity. If convergence: federal coordination may be natural law. If divergence: institutional design is consequential and contingent.',
    'If natural law: Mountain holds. If contingent institutional design: false summit triggers (beneficiaries present) and reclassifies to Tangled Rope or Snare depending on perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_institutional_naturalization, conceptual, 'Whether federal coordination is inherent to industrial society or contingent design').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(1966_johnson_great_society_health_education_expansion, 1965, 1975).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gsedu_tr_t0, 1966_johnson_great_society_health_education_expansion, theater_ratio, 0, 0.28).
narrative_ontology:measurement(gsedu_tr_t5, 1966_johnson_great_society_health_education_expansion, theater_ratio, 5, 0.35).
narrative_ontology:measurement(gsedu_tr_t10, 1966_johnson_great_society_health_education_expansion, theater_ratio, 10, 0.42).

% Extraction over time
narrative_ontology:measurement(gsedu_be_t0, 1966_johnson_great_society_health_education_expansion, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(gsedu_be_t5, 1966_johnson_great_society_health_education_expansion, base_extractiveness, 5, 0.28).
narrative_ontology:measurement(gsedu_be_t10, 1966_johnson_great_society_health_education_expansion, base_extractiveness, 10, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(1966_johnson_great_society_health_education_expansion, resource_allocation).
narrative_ontology:affects_constraint(1966_johnson_great_society_health_education_expansion, medicaid_state_match_burden).
narrative_ontology:affects_constraint(1966_johnson_great_society_health_education_expansion, education_funding_equity_federal_mandate).
narrative_ontology:affects_constraint(1966_johnson_great_society_health_education_expansion, civil_rights_enforcement_via_federal_spending).

% DUAL FORMULATION NOTE:
% The Great Society programs can be decomposed into structurally distinct constraints: (1) Medicare/Medicaid funding mechanism (resource allocation, high base_extractiveness from federal taxation), (2) Title I education funding (coordination of interstate education standards, lower extractiveness), (3) Civil rights enforcement via spending conditionality (organizational enforcement, moderate extractiveness). Each constraint family member has different epsilon values reflecting their primary function and extraction asymmetry. This story captures the unified institutional apparatus; decomposed stories would highlight the distinct coordination and extraction mechanisms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(1966_johnson_great_society_health_education_expansion, institutional, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
