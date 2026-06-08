% ============================================================================
% CONSTRAINT STORY: dependency_lock_in
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-06-08
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dependency_lock_in, []).

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
 *   constraint_id: dependency_lock_in
 *   human_readable: NHS Dependency Lock-In for AI-Guided Healthcare Precision Infrastructure
 *   domain: healthcare_technology_policy/genomic_medicine/ai_governance
 *
 * SUMMARY:
 *   The NHS dependency lock-in for AI-Guided Healthcare Precision (AIGHP)
 *   infrastructure represents a structural tension between the genuine need
 *   for advanced genomic medicine capabilities and the asymmetric extraction
 *   embedded in proprietary vendor relationships. The NHS lacks in-house
 *   capacity to build, deploy, or maintain the computational infrastructure,
 *   AI expertise, and data pipelines required for genomic medicine at scale.
 *   This capacity gap creates dependency on private providers (Palantir,
 *   DeepMind Health, genomic data platforms) whose proprietary systems embed
 *   lock-in through non-interoperable data formats, opaque algorithms, high
 *   switching costs, and contractual terms that retain vendor control over
 *   NHS genomic data. The constraint exhibits genuine coordination (private
 *   expertise enables capabilities the NHS cannot achieve alone) alongside
 *   substantial extraction (vendor lock-in suppresses alternatives, prevents
 *   audit, and concentrates control over public health data in private
 *   hands). Theater ratio (0.48) reflects that procurement governance and
 *   interoperability standards are partially performative: FHIR compliance is
 *   nominal but proprietary extensions persist, audit requirements exist but
 *   cannot penetrate algorithmic opacity, and 'partnership' rhetoric masks
 *   asymmetric power. The constraint is intensifying over the 8-year interval
 *   as more NHS genomic data migrates into proprietary platforms and
 *   switching costs compound.
 *
 * KEY AGENTS:
 *   - NHS Patients: Primary victim (powerless/trapped) — no exit from NHS system, no control over vendor selection, bear full cost of lock-in through reduced care quality or privacy loss
 *   - NHS Procurement Authority: Mixed victim and beneficiary (institutional/constrained) — needs AIGHP infrastructure but constrained by switching costs and lack of in-house capacity; experiences both coordination and extraction
 *   - Private AIGHP Providers: Primary beneficiary (institutional/arbitrage) — capture rents through proprietary lock-in, data retention, and switching costs; global arbitrage exit options
 *   - NHS Clinical Staff: Secondary victim (moderate/constrained) — benefit from diagnostic tools but constrained by system opacity and vendor-imposed workflows
 *   - Alternative Provider Market: Secondary victim (powerless/trapped) — structurally excluded by incumbent lock-in and network effects
 *   - Open Health Data Coalition: Organized agents (organized/mobile) — building open-source alternatives and interoperability standards with sunset logic
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dependency_lock_in, 0.58).
domain_priors:suppression_score(dependency_lock_in, 0.67).
domain_priors:theater_ratio(dependency_lock_in, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dependency_lock_in, extractiveness, 0.58).
narrative_ontology:constraint_metric(dependency_lock_in, suppression_requirement, 0.67).
narrative_ontology:constraint_metric(dependency_lock_in, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dependency_lock_in, tangled_rope).
narrative_ontology:human_readable(dependency_lock_in, "NHS Dependency Lock-In for AI-Guided Healthcare Precision Infrastructure").
narrative_ontology:topic_domain(dependency_lock_in, "healthcare_technology_policy/genomic_medicine/ai_governance").

domain_priors:requires_active_enforcement(dependency_lock_in).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dependency_lock_in, private_aighp_providers).
narrative_ontology:constraint_beneficiary(dependency_lock_in, proprietary_platform_vendors).
narrative_ontology:constraint_beneficiary(dependency_lock_in, data_intermediaries).
narrative_ontology:constraint_victim(dependency_lock_in, nhs_autonomy).
narrative_ontology:constraint_victim(dependency_lock_in, nhs_patients).
narrative_ontology:constraint_victim(dependency_lock_in, public_health_system_sovereignty).
narrative_ontology:constraint_victim(dependency_lock_in, alternative_provider_market).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NHS PATIENTS (SNARE) — Patients have no exit from the NHS system and no visibility into or control over which private vendors process their genomic data. They bear the full cost of vendor lock-in through reduced care quality if proprietary systems fail or extract rents, yet have no agency to switch providers or demand interoperability. Maximum experienced extraction — structurally trapped with no alternative.
constraint_indexing:constraint_classification(dependency_lock_in, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: NHS PROCUREMENT AUTHORITY (TANGLED ROPE) — The NHS genuinely needs AIGHP infrastructure to deliver genomic medicine (coordination function), but lacks in-house capacity to build or maintain it. Procurement contracts with private providers solve a real problem but embed asymmetric extraction through proprietary lock-in, high switching costs, and audit opacity. The NHS has institutional power but constrained exit — switching vendors requires rebuilding data pipelines, retraining staff, and risking service continuity. Mixed beneficiary and victim.
constraint_indexing:constraint_classification(dependency_lock_in, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PRIVATE AIGHP PROVIDERS (ROPE) — Vendors experience the constraint as pure coordination: they provide infrastructure the NHS cannot build itself, enabling genomic medicine deployment. From their perspective, proprietary systems and data retention are legitimate business models, not extraction. They have arbitrage-level exit (can serve other healthcare systems globally) and are net beneficiaries — extraction flows toward them.
constraint_indexing:constraint_classification(dependency_lock_in, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: OPEN HEALTH DATA COALITION (SCAFFOLD) — Organized advocacy groups (OpenSAFELY, NHS Digital transformation initiatives, open-source genomics consortia) see the dependency as a temporary coordination failure with a sunset: public investment in open-source AIGHP infrastructure, interoperability standards (FHIR, GA4GH), and in-house NHS AI capacity-building will create exit pathways within 10-15 years. They experience low extraction because they have agency and see a structural path to dissolving the lock-in.
constraint_indexing:constraint_classification(dependency_lock_in, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: NHS CLINICAL STAFF (TANGLED ROPE) — Clinicians benefit from AIGHP tools that improve diagnostic accuracy and treatment personalization (coordination), but are constrained by proprietary system opacity, lack of audit trails, and vendor-imposed workflow rigidity. They cannot exit to alternative systems without institutional support, and bear career risk if they challenge vendor recommendations. Mixed experience — genuine utility alongside extractive constraints.
constraint_indexing:constraint_classification(dependency_lock_in, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 6: ALTERNATIVE PROVIDER MARKET (SNARE) — Smaller vendors, open-source projects, and academic research groups are structurally excluded by high switching costs, proprietary data formats, and network effects favoring incumbent platforms. They cannot compete on equal terms because NHS data is already locked into proprietary systems, and interoperability is controlled by incumbents. Pure extraction — the lock-in suppresses market alternatives.
constraint_indexing:constraint_classification(dependency_lock_in, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational perspective, the constraint exhibits both genuine coordination (private expertise enables genomic medicine deployment the NHS could not achieve alone) and asymmetric extraction (proprietary lock-in, audit opacity, switching costs, suppression of alternatives). The coordination function is real but the extraction is substantial and structurally embedded. Tangled Rope is the analytically correct classification.
constraint_indexing:constraint_classification(dependency_lock_in, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dependency_lock_in_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(dependency_lock_in, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(dependency_lock_in, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(dependency_lock_in, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(dependency_lock_in_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Substantial. Private vendors capture significant rents through proprietary lock-in, switching costs (estimated at 40-60% of initial deployment cost), data retention clauses, and audit opacity. The NHS pays not just for services but for ongoing dependency — each contract renewal embeds deeper lock-in as more data migrates into proprietary formats. However, extraction is not maximal because genuine coordination value exists: vendors provide expertise and infrastructure the NHS demonstrably lacks. The value reflects asymmetric extraction layered on top of real coordination, not pure rent extraction. Suppression (0.67): High. Switching costs are prohibitive (multi-year data migration, staff retraining, service continuity risk), proprietary data formats prevent interoperability despite nominal FHIR compliance, audit opacity prevents accountability, and network effects favor incumbents. Alternative providers are structurally excluded. However, suppression is not total — open-source initiatives exist, some NHS trusts retain data sovereignty, and regulatory pressure for interoperability is building. Theater ratio (0.48): Moderate. Procurement governance includes audit requirements, interoperability standards, and partnership rhetoric, but much of this is performative: audits cannot penetrate algorithmic black boxes, FHIR compliance is nominal (proprietary extensions dominate), and 'partnership' language masks asymmetric power. The theater is rising over time as governance mechanisms fail to constrain vendor power. All three metrics show upward trajectories, indicating extraction accumulation and enforcement intensification as the dependency deepens.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates the full indexical range from snare (patients, alternative providers) through tangled rope (NHS authority, clinical staff, analytical observer) to rope (private vendors) and scaffold (open data coalition). Patients see pure extraction — they are trapped in a system where private vendors control their genomic data with no accountability. The NHS procurement authority sees mixed coordination and extraction — they need the infrastructure but are locked into extractive terms. Private vendors see pure coordination — they are solving a problem the NHS cannot solve alone. The open data coalition sees a temporary problem with a sunset — open-source infrastructure and interoperability standards will dissolve the lock-in within a generation. Clinical staff see utility constrained by opacity — the tools work but the black-box nature prevents full trust. Alternative providers see structural exclusion — the lock-in suppresses market competition. The analytical observer sees the structural truth: this is a tangled rope, not a rope or a snare. The coordination function is real (the NHS genuinely lacks capacity) but the extraction is substantial and embedded (proprietary lock-in is not a necessary feature of AIGHP deployment).
 *
 * DIRECTIONALITY LOGIC:
 *   NHS patients are full victims with trapped exit — they experience maximum effective extraction because they have no agency and no alternative. The NHS procurement authority is a mixed case: institutional power but constrained exit, declared as both beneficiary (receives AIGHP capabilities) and victim (bears lock-in costs). The engine will derive moderate directionality, producing substantial but not maximal chi. Private AIGHP providers are full beneficiaries with arbitrage exit — they experience the constraint as pure coordination (negative chi). NHS clinical staff are moderate-power victims with constrained exit — they experience substantial extraction but have some agency. The alternative provider market is powerless victims with trapped exit — they experience maximum extraction through structural exclusion. The open health data coalition is organized with mobile exit — they experience low extraction because they have agency and see an exit path. The analytical observer sees the structural reality: genuine coordination layered with asymmetric extraction, producing Tangled Rope classification.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves the mandatrophy by demonstrating that tangled rope is the structurally correct classification when genuine coordination and asymmetric extraction coexist. The NHS genuinely needs AIGHP infrastructure to deliver genomic medicine — this is not a false coordination story. But the proprietary lock-in, audit opacity, switching costs, and suppression of alternatives are extractive mechanisms layered on top of the coordination function. The mandate (deploy genomic medicine) has not outlived its function, but the execution (proprietary vendor dependency) embeds extraction that is not necessary to achieve the mandate. Open-source alternatives, interoperability standards, and in-house capacity investment could achieve the same coordination function with lower extraction. The tangled rope classification captures this structural reality: the constraint is neither pure coordination (rope) nor pure extraction (snare), but a hybrid where both functions coexist and must be measured independently.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    in_house_capacity_feasibility,
    'Could the NHS realistically build and maintain in-house AIGHP infrastructure at scale, or does genuine technical complexity require private sector expertise?',
    'Comparative analysis of public vs. private AI infrastructure projects in healthcare (e.g., NHS Digital vs. Palantir Foundry deployments); cost-benefit analysis of in-house capacity investment vs. procurement; assessment of NHS AI talent retention capacity',
    'If feasible: the dependency is a policy choice (higher extractiveness, snare from more perspectives). If infeasible: the dependency reflects genuine coordination need (lower extractiveness, rope from more perspectives).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(in_house_capacity_feasibility, empirical, 'Whether NHS could build in-house AIGHP capacity').

omega_variable(
    interoperability_standard_adoption,
    'Will open interoperability standards (FHIR, GA4GH) actually enable NHS to switch AIGHP vendors without prohibitive cost, or do proprietary extensions and data lock-in persist despite nominal standard compliance?',
    'Longitudinal tracking of NHS vendor switching attempts; measurement of actual switching costs vs. projected costs; analysis of proprietary extension prevalence in FHIR implementations',
    'If standards enable switching: scaffold perspective confirmed, sunset is real. If proprietary lock-in persists: standards are theater, extraction is structural.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(interoperability_standard_adoption, empirical, 'Whether interoperability standards reduce switching costs').

omega_variable(
    audit_opacity_necessity,
    'Is proprietary system opacity (inability to audit AIGHP algorithms and data flows) a necessary protection of trade secrets, or an extractive mechanism that prevents accountability?',
    'Comparison of audit transparency in open-source vs. proprietary AIGHP systems; assessment of whether trade secret protection requires full opacity or whether partial transparency (e.g., model cards, audit APIs) is feasible; analysis of regulatory audit requirements in other jurisdictions',
    'If opacity is necessary: lower extractiveness (legitimate coordination cost). If opacity is extractive: higher suppression (prevents accountability and exit).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(audit_opacity_necessity, conceptual, 'Whether audit opacity is necessary or extractive').

omega_variable(
    data_sovereignty_threshold,
    'At what threshold of genomic data held by private entities does NHS lose effective sovereignty over public health policy, and is that threshold already crossed?',
    'Quantitative analysis of NHS genomic data distribution (public vs. private holdings); assessment of policy leverage loss at different data concentration levels; historical case studies of data-driven policy capture',
    'If threshold not crossed: dependency is reversible (scaffold logic holds). If threshold crossed: dependency is structural lock-in (snare from more perspectives).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(data_sovereignty_threshold, empirical, 'Data concentration threshold for sovereignty loss').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dependency_lock_in, 0, 8).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dep_lock_theater_t0, dependency_lock_in, theater_ratio, 0, 0.3).
narrative_ontology:measurement(dep_lock_theater_t2, dependency_lock_in, theater_ratio, 2, 0.35).
narrative_ontology:measurement(dep_lock_theater_t4, dependency_lock_in, theater_ratio, 4, 0.4).
narrative_ontology:measurement(dep_lock_theater_t6, dependency_lock_in, theater_ratio, 6, 0.45).
narrative_ontology:measurement(dep_lock_theater_t8, dependency_lock_in, theater_ratio, 8, 0.48).

% Extraction over time
narrative_ontology:measurement(dep_lock_extract_t0, dependency_lock_in, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(dep_lock_extract_t2, dependency_lock_in, base_extractiveness, 2, 0.42).
narrative_ontology:measurement(dep_lock_extract_t4, dependency_lock_in, base_extractiveness, 4, 0.5).
narrative_ontology:measurement(dep_lock_extract_t6, dependency_lock_in, base_extractiveness, 6, 0.55).
narrative_ontology:measurement(dep_lock_extract_t8, dependency_lock_in, base_extractiveness, 8, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(dep_lock_suppress_t0, dependency_lock_in, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(dep_lock_suppress_t2, dependency_lock_in, suppression_requirement, 2, 0.52).
narrative_ontology:measurement(dep_lock_suppress_t4, dependency_lock_in, suppression_requirement, 4, 0.6).
narrative_ontology:measurement(dep_lock_suppress_t6, dependency_lock_in, suppression_requirement, 6, 0.65).
narrative_ontology:measurement(dep_lock_suppress_t8, dependency_lock_in, suppression_requirement, 8, 0.67).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dependency_lock_in, global_infrastructure).
narrative_ontology:affects_constraint(dependency_lock_in, algorithmic_opacity).
narrative_ontology:affects_constraint(dependency_lock_in, data_sovereignty_erosion).

% DUAL FORMULATION NOTE:
% The dependency lock-in is downstream of scientific_viability_uncertainty (the NHS would not deploy AIGHP infrastructure if the underlying genomic medicine science were not viable) but represents a distinct structural constraint. The upstream constraint has its own extractiveness reflecting the contested empirical status of AIGHP efficacy; the dependency lock-in has its own extractiveness reflecting the vendor power asymmetry and proprietary lock-in mechanisms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
