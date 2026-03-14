% ============================================================================
% CONSTRAINT STORY: geographic_healthcare_access_disparity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_geographic_healthcare_access_disparity, []).

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
 *   constraint_id: geographic_healthcare_access_disparity
 *   human_readable: Geographic Healthcare Access Disparity
 *   domain: health/economic/social
 *
 * SUMMARY:
 *   Geographic healthcare access disparity refers to the structural gap
 *   between healthcare availability and quality in urban vs rural regions.
 *   This constraint operates as a hybrid coordination-extraction mechanism:
 *   genuine coordination problems exist (rural communities need local
 *   healthcare, economies of scale justify some concentration), but the
 *   allocation system simultaneously extracts from rural populations through
 *   reimbursement structures, licensing restrictions, regulatory burdens, and
 *   market consolidation that preserve geographic inequality. The disparity
 *   is not immutable (it varies dramatically across countries with different
 *   healthcare policies) nor is it incidental (it is actively maintained by
 *   institutional mechanisms). The constraint exhibits all perspectives from
 *   snare (for trapped rural patients with no exit) to rope (for urban
 *   beneficiaries) to scaffold (for digital health alternatives with sunset
 *   logic). The theater ratio (0.55) reflects that rural healthcare systems
 *   spend significant effort maintaining compliance and institutional
 *   legitimacy despite declining functional capacity — many rural hospitals
 *   are inertially maintained through subsidies and regulatory protection
 *   rather than genuine service provision.
 *
 * KEY AGENTS:
 *   - Rural Patients: Primary victim (powerless/trapped) — geographically immobilized by employment, family, or property; bear full cost of disparity through delayed care, preventable mortality, and travel burden
 *   - Rural Healthcare Workers: Secondary victim (moderate/constrained) — constrained by limited job mobility and professional ties; experience genuine coordination (communities need care) alongside extraction (inadequate compensation, isolation, burnout)
 *   - Urban Healthcare Providers: Primary beneficiary (institutional/arbitrage) — concentrate resources, capture specialist market, control allocation mechanisms; experience constraint as coordination rather than extraction
 *   - Telemedicine Coalition: Organized alternative actors (organized/mobile) — building technological pathways to bypass geographic constraints; represent sunset mechanism
 *   - Rural Hospital Systems: Inertial institutional forms (institutional/arbitrage) — maintain zombie functionality through subsidies and regulatory protection; high theater ratio indicating degraded primary function
 *   - Public Health Agencies: Coordinating institutions (organized/constrained) — simultaneously coordinate public health goals and maintain extractive allocation structures; locked into budget cycles and political economy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(geographic_healthcare_access_disparity, 0.58).
domain_priors:suppression_score(geographic_healthcare_access_disparity, 0.68).
domain_priors:theater_ratio(geographic_healthcare_access_disparity, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(geographic_healthcare_access_disparity, extractiveness, 0.58).
narrative_ontology:constraint_metric(geographic_healthcare_access_disparity, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(geographic_healthcare_access_disparity, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(geographic_healthcare_access_disparity, tangled_rope).
narrative_ontology:human_readable(geographic_healthcare_access_disparity, "Geographic Healthcare Access Disparity").
narrative_ontology:topic_domain(geographic_healthcare_access_disparity, "health/economic/social").

domain_priors:requires_active_enforcement(geographic_healthcare_access_disparity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(geographic_healthcare_access_disparity, urban_healthcare_providers).
narrative_ontology:constraint_beneficiary(geographic_healthcare_access_disparity, pharmaceutical_companies).
narrative_ontology:constraint_beneficiary(geographic_healthcare_access_disparity, medical_equipment_distributors).
narrative_ontology:constraint_victim(geographic_healthcare_access_disparity, rural_populations).
narrative_ontology:constraint_victim(geographic_healthcare_access_disparity, geographically_isolated_communities).
narrative_ontology:constraint_victim(geographic_healthcare_access_disparity, low_income_mobile_workers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RURAL PATIENT (SNARE) — Trapped in geographic location by employment, property ownership, family ties, or economic dependency. No viable exit options. Bears full cost of healthcare disparity: higher travel burden, delayed diagnoses, preventable mortality, lack of specialist access. Maximum experienced extraction with no alternatives.
constraint_indexing:constraint_classification(geographic_healthcare_access_disparity, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: RURAL HEALTHCARE WORKER (TANGLED ROPE) — Constrained by limited job opportunities outside healthcare, professional licensure tied to location, and cultural ties to community. Experiences genuine coordination problem (rural communities need local healthcare) alongside extraction (low reimbursement rates, long hours, limited specialist backup, workforce burnout). Benefits from coordination of care delivery and community trust, but bears asymmetric extraction through inadequate compensation and professional isolation.
constraint_indexing:constraint_classification(geographic_healthcare_access_disparity, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: URBAN HEALTHCARE PROVIDERS (ROPE) — Primary beneficiaries. Experiences the constraint as coordination: concentration of resources in urban centers enables specialized care, teaching hospitals, and research infrastructure. Low extraction experienced because they control the allocation mechanism. Net beneficiary — extraction flows toward them. Arbitrage options available: can expand services, consolidate markets, or relocate operations.
constraint_indexing:constraint_classification(geographic_healthcare_access_disparity, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: TELEMEDICINE AND DIGITAL HEALTH COALITION (SCAFFOLD) — Organized actors (telehealth platforms, community health networks, digital health startups) see the geographic disparity as a solvable coordination problem with technological sunset. Remote diagnosis, asynchronous specialist consultation, and distributed care coordination reduce the geographic extraction. Coalition has agency and exit strategy: digital infrastructure can eventually bypass traditional geographic constraints. Effective extraction decreases as digital pathways mature.
constraint_indexing:constraint_classification(geographic_healthcare_access_disparity, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: RURAL HOSPITAL SYSTEM (PITON) — Many rural hospitals persist as institutional zombies: maintained through federal subsidies, regulatory protections, and community identity despite declining functionality. Theater ratio high (50-70%): staff spend time documenting compliance, maintaining accreditation, and navigating regulatory requirements that exceed actual service capacity. Primary function (provide local care) has atrophied; institutional shell persists through inertia. Arbitrage options exist but are constrained by community dependency and regulatory barriers to consolidation.
constraint_indexing:constraint_classification(geographic_healthcare_access_disparity, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: PUBLIC HEALTH AGENCIES (TANGLED ROPE) — Organized institutional actors that both coordinate and extract. They coordinate genuine public health goals (universal access, disease prevention, equity) but simultaneously maintain extractive structures (reimbursement formulas that disadvantage rural providers, regulatory barriers to task-shifting, centralized procurement systems that increase rural costs). Constrained by budget cycles, political economy of healthcare systems, and institutional lock-in. Experience both the coordination function and asymmetric extraction simultaneously.
constraint_indexing:constraint_classification(geographic_healthcare_access_disparity, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (SNARE) — From civilizational/global scope, geographic healthcare disparity is not natural or inevitable but a structural extraction mechanism maintained by political economy of healthcare markets. Capital concentration in urban centers, reimbursement structures that favor high-volume procedures, licensing regulations that restrict provider mobility, and market consolidation all actively create and sustain the disparity. The constraint classifies as snare from the analytical perspective: the extraction is neither incidental nor coordinate-dependent, but structurally central to how contemporary healthcare systems allocate resources and concentrate wealth.
constraint_indexing:constraint_classification(geographic_healthcare_access_disparity, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(geographic_healthcare_access_disparity_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(geographic_healthcare_access_disparity, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(geographic_healthcare_access_disparity, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(geographic_healthcare_access_disparity, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(geographic_healthcare_access_disparity, TR),
    TR >= 0.70.

:- end_tests(geographic_healthcare_access_disparity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts from rural populations through multiple mechanisms: (1) reimbursement formulas that provide less revenue per capita to rural providers, (2) licensing and regulatory barriers that restrict provider mobility, (3) pharmaceutical/medical device supply chains optimized for urban economies of scale, (4) concentration of specialized care and research infrastructure in urban centers. The extraction is systematic rather than accidental — it emerges from healthcare market structures and is maintained by institutional incentives. However, extraction is not total because some coordination functions are genuine (economies of scale are real, specialization requires concentration, some geographic variation is efficient). Suppression (0.68): High. Multiple suppression mechanisms operate: (1) material barriers (distance, travel cost, time burden), (2) structural barriers (reimbursement limits rural provider supply, licensing restricts task-shifting, regulatory compliance burden exceeds rural hospital capacity), (3) internalized acceptance (rural populations often accept healthcare inequality as inevitable), (4) political economy (rural areas have less political power to demand resource allocation). Theater ratio (0.55): Moderate. Rural healthcare systems spend significant effort on compliance documentation, accreditation maintenance, and regulatory navigation that exceeds their functional capacity. Many rural hospitals persist as institutional shells through subsidies rather than service viability. However, theater is not dominant (>0.70) because actual care delivery still occurs — the zombie hospitals do provide services, just inadequately. The theater has increased over the interval (0.38 → 0.55) as regulatory complexity increased without corresponding capacity growth.
 *
 * PERSPECTIVAL GAP:
 *   Maximum gap between powerless victim (Snare) and institutional beneficiary (Rope). The victim experiences the constraint as purely extractive with no coordination benefit. The beneficiary experiences coordination (solving the genuine problem of specialization and economy of scale). The gap reveals that the classification depends entirely on structural position: are you bearing costs without benefits (Snare), or capturing benefits while externalizing costs (Rope)? The rural patient cannot see the coordination function because they only experience extraction. The urban provider cannot see the extraction because they only experience coordination. The analytical observer can see both: coordination functions are real, but they are asymmetrically distributed — benefits concentrate in urban centers while costs concentrate in rural areas.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality (d) is derived from power level, exit options, and structural relationship to extraction flow. Trapped rural patients with no exit (d ≈ 0.95) experience maximum f(d) ≈ 1.42, producing high experienced extraction (χ ≈ 0.82). Urban institutional beneficiaries with arbitrage options (d ≈ 0.05) experience negative f(d) ≈ -0.12, producing negative χ — they see the system as enabling rather than extracting. Constrained rural healthcare workers (d ≈ 0.75) experience moderate f(d) ≈ 1.15. Organized actors with mobile options (d ≈ 0.60) experience moderate f(d) ≈ 0.75. The directionality derivation captures how institutional position determines experienced extractiveness: the same constraint system produces vastly different χ values depending on whether the agent is trapped, constrained, or has arbitrage options.
 *
 * MANDATROPHY ANALYSIS:
 *   STRUCTURE: This constraint resolves the mandatrophy by showing that geographic healthcare disparity operates as a genuine tangled rope — it coordinates real functions (specialization, economies of scale, research infrastructure) while simultaneously extracting from dispersed populations (rural patients, rural workers) through institutional mechanisms that preserve geographic inequality. The constraint is neither pure coordination (rope) nor pure extraction (snare), but a hybrid where coordination and extraction are inseparable. BENEFICIARY-VICTIM: Urban healthcare providers and specialized facilities benefit from concentration (capturing specialist market, economies of scale, research funding concentration). Rural populations bear costs (delayed care, preventable mortality, higher out-of-pocket burden, professional isolation for rural workers). PUBLIC HEALTH COORDINATION: The coordination function is real — concentrating specialized care improves outcomes for complex cases, reduces per-case costs for high-volume procedures, enables research infrastructure and teaching. However, this coordination is not the only possible arrangement: countries with more distributed healthcare systems (Germany's regional hospital networks, Japan's distributed specialist capacity, Cuba's community health worker model) achieve comparable or better outcomes while reducing geographic extraction. INSTITUTIONAL ENFORCEMENT: The constraint is actively enforced through reimbursement formulas, licensing regulations, and market consolidation — it is not a passive outcome of efficiency. This active enforcement distinguishes Tangled Rope (requires enforcement) from Rope (self-sustaining coordination). SUNSET TEST: The constraint shows no sunset trajectory. Unlike scaffolds, which have explicit sunset clauses and declining theater ratios, geographic healthcare disparity has stable or increasing extraction over time. The telemedicine coalition represents a potential sunset mechanism, but current evidence suggests this will create a hybrid (some telehealth for routine care, continued geographic concentration for specialized/emergency) rather than full replacement. THEATER: Moderate but rising (0.55), indicating some performative elements in rural hospital maintenance but not dominant dysfunction. Theater is lower than piton thresholds because rural hospitals do provide actual care, albeit inadequately.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_vs_constructed_disparity,
    'Is the geographic healthcare disparity a natural consequence of population density and economic efficiency, or a constructed extraction mechanism?',
    'Historical comparison of healthcare systems with different geographic allocation policies; analysis of countries with stronger rural healthcare investment and their outcomes; examination of whether disparity persists when reimbursement formulas are equalized',
    'If natural: constraint classifies as Mountain (immutable feature of healthcare economics). If constructed: constraint classifies as Snare (extractive maintenance of disparity). Current evidence suggests constructed but with genuine coordination components.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_vs_constructed_disparity, empirical, 'Whether disparity is natural or constructed').

omega_variable(
    telehealth_true_capability,
    'Can telemedicine and digital health infrastructure genuinely replace geographic proximity for most healthcare functions, or are specialized/emergency functions irreducibly tied to physical location?',
    'Comparative outcomes analysis: telehealth vs in-person for rural populations; identification of care functions that cannot be delivered remotely; analysis of whether digital infrastructure reduces mortality differentials',
    'If capable: scaffold perspective is accurate, sunset is real. If limited: telehealth provides marginal improvement but does not resolve the core geographic constraint. Current data suggests mixed: effective for chronic disease management and specialist consultation, inadequate for emergency surgery, trauma, and complex in-person diagnostics.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(telehealth_true_capability, empirical, 'Whether telemedicine can replace geographic proximity').

omega_variable(
    extraction_or_rational_allocation,
    'Do economies of scale and specialization justify resource concentration in urban centers, or does the allocation mechanism extract beyond rational efficiency?',
    'Cost-benefit analysis of urban concentration vs distributed capacity; examination of whether concentrated systems achieve better health outcomes per capita; identification of where concentration increases costs without improving outcomes',
    'If rational: constraint is coordination problem with minor extraction overhead (Rope). If excessive: constraint is primarily extractive with efficiency justifications as cover (Tangled Rope or Snare). Evidence suggests both: some concentration is efficient, but current levels exceed efficiency gains and include extractive elements.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_or_rational_allocation, empirical, 'Whether concentration allocation is rational or extractive').

omega_variable(
    suppression_mechanism_identity_lock,
    'Is rural healthcare access disparity suppressed primarily through material barriers (cost, distance, equipment), structural barriers (licensing, reimbursement), or internalized acceptance (rural populations accepting healthcare inequality as inevitable)?',
    'Survey data on rural population health literacy and expectations; analysis of rural activism for healthcare access vs acceptance; longitudinal tracking of suppression levels when material barriers are removed',
    'If material/structural: removing barriers changes outcomes. If internalized: rural populations may not seek access even when barriers are removed. True suppression likely combines all three — material barriers create conditions for internalized acceptance, which reinforces structural barriers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_identity_lock, empirical, 'Suppression mechanism: material, structural, or internalized').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(geographic_healthcare_access_disparity, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(geo_healthcare_tr_t0, geographic_healthcare_access_disparity, theater_ratio, 0, 0.38).
narrative_ontology:measurement(geo_healthcare_tr_t10, geographic_healthcare_access_disparity, theater_ratio, 10, 0.48).
narrative_ontology:measurement(geo_healthcare_tr_t20, geographic_healthcare_access_disparity, theater_ratio, 20, 0.55).

% Extraction over time
narrative_ontology:measurement(geo_healthcare_be_t0, geographic_healthcare_access_disparity, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(geo_healthcare_be_t10, geographic_healthcare_access_disparity, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(geo_healthcare_be_t20, geographic_healthcare_access_disparity, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(geographic_healthcare_access_disparity, resource_allocation).
narrative_ontology:affects_constraint(geographic_healthcare_access_disparity, healthcare_workforce_brain_drain).
narrative_ontology:affects_constraint(geographic_healthcare_access_disparity, rural_hospital_closure_cascade).
narrative_ontology:affects_constraint(geographic_healthcare_access_disparity, maternal_mortality_geographic_gap).

% DUAL FORMULATION NOTE:
% Geographic healthcare access disparity decomposes into multiple constraint families: (1) resource allocation disparity (ε=0.58, Tangled Rope) — this story; (2) healthcare workforce brain drain (ε=0.62, Snare) — incentive structure pulling healthcare workers from rural areas; (3) rural hospital closure cascade (ε=0.51, Tangled Rope) — institutional feedback loop where low reimbursement causes closures which increase disparity which reduces reimbursement; (4) maternal mortality geographic gap (ε=0.72, Snare) — specific health outcome disparity with irreducible geographic component. Each has different ε and requires separate analysis, but all are downstream of the underlying geographic allocation structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
