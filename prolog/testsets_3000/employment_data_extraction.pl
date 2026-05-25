% ============================================================================
% CONSTRAINT STORY: employment_data_extraction
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_employment_data_extraction, []).

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
 *   constraint_id: employment_data_extraction
 *   human_readable: Employment Data Extraction and Worker Surveillance
 *   domain: labor/digital_rights/data_governance
 *
 * SUMMARY:
 *   Employment data extraction is a constraint spanning hiring, workplace
 *   management, performance evaluation, and post-employment record-keeping.
 *   Workers surrender personal data as a condition of employment and have no
 *   meaningful exit options — refusal to provide data results in employment
 *   denial or termination. The constraint exhibits genuine coordination
 *   functions (matching worker capability to roles, ensuring workplace
 *   safety, administering benefits and wage systems) alongside asymmetric
 *   extraction (surveillance enables wage suppression, algorithmic
 *   discrimination, retaliation against organizing, and post-employment
 *   exclusion). The extractiveness has increased over the interval as digital
 *   surveillance technologies have enabled continuous monitoring at declining
 *   cost. Theater ratio remains low because workplace surveillance is
 *   functionally integrated into management systems rather than purely
 *   ceremonial — but privacy regulations have added performative compliance
 *   (GDPR consent forms, CCPA data access requests) without substantively
 *   limiting extraction, suggesting piton dynamics may be emerging. The
 *   constraint demonstrates how a single structural arrangement can be
 *   experienced as coordination (rope) by beneficiaries, as coercive
 *   extraction (snare) by workers, as a mixed governance problem (tangled
 *   rope) by regulators, and as an immutable feature of capitalism (false
 *   mountain) by those who naturalize labor relations.
 *
 * KEY AGENTS:
 *   - Surveilled Workers: Primary victims (powerless/trapped) — mandatory participation in employment surveillance systems; no exit without labor market withdrawal
 *   - Job Applicants: Primary victims (powerless/trapped) — must surrender extensive personal data to access employment; refusal results in automatic rejection
 *   - Employer Analytics Divisions: Primary beneficiaries (institutional/arbitrage) — extract real-time productivity and capability data; experience as coordination mechanism
 *   - Data Brokers: Secondary beneficiaries (institutional/arbitrage) — aggregate and resell employment data; low perceived extraction (consensual market framing)
 *   - Labor Regulators: Moderate-power actors (moderate/constrained) — face coordination problem (employment markets need some data) and extraction problem (same data enable discrimination); constrained by political economy
 *   - Worker-Protective Coalition: Organized actors (powerful/constrained) — unions, advocacy orgs, worker-protective regulators; have power but face employer counter-pressure and worker fear
 *   - Privacy Regulators: Institutional actors (institutional/arbitrage) — implement GDPR, CCPA, LGPD; maintain performative compliance theater while extraction persists
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional arrangements (surveillance tech, algorithmic management) as inherent labor relations
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(employment_data_extraction, 0.58).
domain_priors:suppression_score(employment_data_extraction, 0.65).
domain_priors:theater_ratio(employment_data_extraction, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(employment_data_extraction, extractiveness, 0.58).
narrative_ontology:constraint_metric(employment_data_extraction, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(employment_data_extraction, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(employment_data_extraction, tangled_rope).
narrative_ontology:human_readable(employment_data_extraction, "Employment Data Extraction and Worker Surveillance").
narrative_ontology:topic_domain(employment_data_extraction, "labor/digital_rights/data_governance").

domain_priors:requires_active_enforcement(employment_data_extraction).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(employment_data_extraction, employer_analytics_divisions).
narrative_ontology:constraint_beneficiary(employment_data_extraction, data_brokers).
narrative_ontology:constraint_beneficiary(employment_data_extraction, algorithmic_management_platforms).
narrative_ontology:constraint_victim(employment_data_extraction, workers_subject_to_surveillance).
narrative_ontology:constraint_victim(employment_data_extraction, job_applicants).
narrative_ontology:constraint_victim(employment_data_extraction, worker_privacy_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SURVEILLED WORKER (SNARE) — Workers cannot exit employment data extraction without losing employment itself. Workplace surveillance (keystroke monitoring, location tracking, productivity scoring, email content analysis) is compulsory for job retention. No alternative employment pathway avoids similar data extraction. Exit options are constrained to complete labor market withdrawal. Maximum experienced extraction.
constraint_indexing:constraint_classification(employment_data_extraction, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: JOB APPLICANT (SNARE) — Applicants must surrender extensive personal data (work history, social media profiles, psychometric assessments, background checks, credit reports) to access employment. Refusal to provide data results in automatic rejection. No alternative hiring mechanism exists that avoids data extraction. Trapped in the applicant role.
constraint_indexing:constraint_classification(employment_data_extraction, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: LABOR REGULATOR (TANGLED ROPE) — Regulators face genuine coordination problem: employment markets require some data collection for safety, benefits administration, and wage enforcement. But the same enforcement apparatus enables extraction (worker surveillance feeds disciplinary action, data collection feeds algorithmic discrimination). Regulators are both solving coordination problems and managing extraction mechanisms. High cost to exit regulation (labor market collapse) but some countervailing power through legislative authority.
constraint_indexing:constraint_classification(employment_data_extraction, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: EMPLOYER ANALYTICS DIVISION (ROPE) — Experiences data extraction as coordination: surveillance systems provide real-time productivity metrics, workflow optimization, and risk management. The constraint from their perspective solves coordination problems (matching worker capability to task demand, detecting safety violations early, allocating work efficiently). Net beneficiary with arbitrage exit options (can license analytics platforms, migrate to cloud vendors, switch surveillance technologies). Low effective extraction experienced by beneficiary.
constraint_indexing:constraint_classification(employment_data_extraction, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: DATA BROKER ECOSYSTEM (ROPE) — Data brokers aggregate employment data (job history, salary, location, performance ratings, termination records) and resell to employers, insurers, credit agencies. From their perspective, this is pure coordination — matching employment information supply to demand across markets. They experience zero suppression (consensual data markets, no coercion needed) and high arbitrage (can switch clients, diversify data sources, integrate new streams). Minimal extraction experienced.
constraint_indexing:constraint_classification(employment_data_extraction, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: WORKER-PROTECTIVE COALITION (TANGLED ROPE) — Labor unions, worker advocacy organizations, and data-protective regulators see both coordination value (employment matching, safety systems) and extraction (invasive surveillance, algorithmic bias, data commodification). They have power through collective action and regulatory influence but face high suppression from employer counter-pressure, worker fear of retaliation, and fragmented worker agency. Mixed experience of coordination benefit and extraction cost; constrained by political economy factors.
constraint_indexing:constraint_classification(employment_data_extraction, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: PRIVACY REGULATION THEATER (PITON) — Privacy regulations (GDPR, CCPA, LGPD) create performative compliance rituals: privacy policies, consent forms, data access requests. But the theater does not functionally limit data extraction — workers still cannot opt out of workplace surveillance without job loss; employers still extract data during hiring; the systems persist unchanged. Compliance becomes a ritual rather than a functional constraint. Theater ratio ≥ 0.70 indicates this is degraded from a coordination mechanism that might actually protect worker privacy.
constraint_indexing:constraint_classification(employment_data_extraction, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal/civilizational perspective, some employment data collection appears immutable: employers have always needed to assess worker capability and output; labor markets have always required information asymmetry resolution; capital has always sought to monitor returns on investment. This perspective sees employment data extraction as an inherent feature of labor relations. However, the analytical observer must recognize this as a risk of false summit — the data extraction mechanisms are contingent institutional arrangements (surveillance technology, algorithmic management, data commodification), not laws of nature.
constraint_indexing:constraint_classification(employment_data_extraction, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(employment_data_extraction_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(employment_data_extraction, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(employment_data_extraction, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(employment_data_extraction, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(employment_data_extraction, TR),
    TR >= 0.70.

:- end_tests(employment_data_extraction_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. Employment data extraction has increased significantly as surveillance technologies matured. The constraint combines genuine coordination functions (workplace safety, capability matching, benefits administration — justified ~0.15 baseline) with asymmetric extraction (wage suppression through productivity scoring, algorithmic discrimination, retaliation risk — adds ~0.43 overhead). Total 0.58 reflects that the coordination is real but substantially instrumentalized for extraction. Suppression (0.65): High. Workers face multiple barriers to refusing data extraction: employment-contingent extraction (refusal = job loss), labor market concentration (few employers, similar extraction practices everywhere), fear of retaliation, limited information about what data are collected, and legal/contractual employment terms that normalize surveillance. Suppression is structural, not merely circumstantial. Theater ratio (0.48): Moderate-low. Workplace surveillance is functionally integrated (real-time productivity tracking, algorithmic work assignment) rather than purely ceremonial. However, privacy regulations have added performative elements (consent forms, data access requests) that create appearance of control without substantively limiting extraction. Theater is increasing as regulatory compliance requirements accumulate.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits maximum perspectival divergence. Employer analytics divisions see pure coordination (Rope) — surveillance systems solve real management problems. Data brokers see voluntary markets (Rope) — employment information exchange is consensual. Workers see pure extraction (Snare) — surveillance is mandatory and inescapable. Regulators see a governance problem requiring balance (Tangled Rope) — genuine coordination needs must be reconciled with worker protection. Privacy regulators see a theater problem (Piton) — compliance rituals persist without functional effect. The analytical observer risks seeing natural law (Mountain) — employment has always required information asymmetries — but this is a false summit that naturalizes contingent design choices (surveillance technology choices, algorithmic parameters, data retention policies). The perspectival gap reveals that 'employment coordination' is ideological framing for what workers experience as coercive extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary flow: Employers and data brokers extract value from worker data. The extraction stream runs from workers (victims) to employers (beneficiaries). Workers cannot redirect this stream — they cannot demand compensation for surveillance data, cannot sell their own productivity data, cannot use their surveillance records for their own benefit. Regulators are asymmetrically positioned: they benefit from employment market function (economic productivity, tax revenue, labor market matching) but are obligated to protect workers. The regulatory role is captured when employer interests dominate (common). Worker-protective coalitions benefit from collective worker power but face suppression from employer counter-pressure. The directionality is fundamentally asymmetric except in jurisdictions where worker power is sufficient to reshape it.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The constraint is classified as Tangled Rope at the analytical level because it exhibits both genuine coordination (employment matching requires information; safety systems require data) and asymmetric extraction (surveillance enables wage suppression and discrimination). The misclassification risk is in the direction of pure Snare (if we ignore coordination) or pure Rope (if we ignore extraction). The mandatrophy is resolved by the presence of both beneficiaries (employers, data brokers) who experience coordination benefits and victims (workers) who experience pure extraction, combined with active enforcement (employment contracts, technical surveillance systems) required to sustain the asymmetry. If the coordination function were removed (alternative employment matching mechanisms existed), the constraint would clearly be pure Snare. If the extraction mechanisms were removed (data anonymization, worker data ownership), the constraint would be pure Rope. The tangled_rope classification indicates that both mechanisms are structurally necessary to the constraint's persistence, and that the coordination function serves as ideological cover for extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    consent_versus_coercion_boundary,
    'At what point does employment-contingent data extraction become coercive rather than consensual?',
    'Comparative analysis of turnover rates and wage changes when surveillance is transparent vs hidden; measurement of alternative employment availability for workers refusing surveillance; jurisdictional variation in worker exit costs across regulatory regimes',
    'If employment is genuinely voluntary: constraint remains Tangled Rope with meaningful worker agency. If workers face genuine job market alternatives: constraint could degrade toward Rope (coordination). If exit is impossible: constraint is pure Snare regardless of consent framing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consent_versus_coercion_boundary, empirical, 'Whether employment-contingent data extraction is coercive or consensual').

omega_variable(
    algorithmic_discrimination_necessity,
    'Does employment data extraction genuinely require the algorithmic discrimination outcomes observed, or are those outcomes contingent design choices?',
    'Comparison of hiring outcomes across algorithms with identical data inputs but different parameterization; analysis of whether discrimination patterns emerge from data correlation or from explicit optimization toward group-correlated proxies',
    'If discrimination is algorithmic necessity: extraction is tied to coordination function (matching). If discrimination is contingent design choice: extraction is pure overhead (Snare classification strengthens). If discrimination is outcome of biased training data: constraint decomposes into data quality + algorithm design constraints.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(algorithmic_discrimination_necessity, empirical, 'Whether observed algorithmic discrimination is necessary or contingent').

omega_variable(
    data_retention_justification,
    'What proportion of employment data retention (e.g., permanent employment records, performance scoring archives, terminated-worker data) serves coordination functions vs pure extractive record-keeping?',
    'Audit of data lifecycle: which data are used for active employment coordination; which are retained for post-employment purposes (credit scoring, background checks, re-hiring exclusion); correlation between retention duration and stated business purpose',
    'If retention is necessary for coordination: extraction metrics should reflect coordination costs. If retention is pure historical record-keeping enabling future exploitation: this is a distinct constraint (employment_historical_record_commodification) deserving decomposition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(data_retention_justification, empirical, 'Data retention duration justification: coordination vs archive').

omega_variable(
    worker_collective_power_threshold,
    'What level of worker organization and regulatory support would shift the worker perspective from Snare to Tangled Rope or Rope?',
    'Historical comparison across jurisdictions with different union density, worker protections, and surveillance regulation; analysis of cases where worker-protective coalitions successfully limited data extraction',
    'If threshold is low (achievable union density): constraint is contextual rather than structural. If threshold is high (requires systemic transformation): constraint is structurally embedded. Workers moving from powerless to organized would reclassify from Snare to Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(worker_collective_power_threshold, empirical, 'Worker collective power threshold for reclassification').

omega_variable(
    privacy_regulation_effectiveness,
    'Do existing privacy regulations (GDPR, CCPA, LGPD) actually limit employment data extraction, or are they purely performative?',
    'Measurement of data extraction volume before and after regulation; audit of compliance burden vs actual behavioral change; comparison of worker experience across regulated vs unregulated jurisdictions; analysis of regulatory enforcement capacity vs compliance cost for firms',
    'If effective: regulations degrade the constraint from Snare toward Tangled Rope (worker agency increases). If purely performative: theater_ratio remains ≥ 0.70 and the piton classification is confirmed. If ineffective due to enforcement gaps: regulation becomes a false summit (appears to protect but doesn''t).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(privacy_regulation_effectiveness, empirical, 'Effectiveness of privacy regulation in limiting employment data extraction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(employment_data_extraction, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(empdata_tr_t0, employment_data_extraction, theater_ratio, 0, 0.25).
narrative_ontology:measurement(empdata_tr_t5, employment_data_extraction, theater_ratio, 5, 0.38).
narrative_ontology:measurement(empdata_tr_t10, employment_data_extraction, theater_ratio, 10, 0.48).

% Extraction over time
narrative_ontology:measurement(empdata_be_t0, employment_data_extraction, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(empdata_be_t5, employment_data_extraction, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(empdata_be_t10, employment_data_extraction, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(employment_data_extraction, resource_allocation).
narrative_ontology:affects_constraint(employment_data_extraction, algorithmic_hiring_discrimination).
narrative_ontology:affects_constraint(employment_data_extraction, wage_surveillance_suppression).
narrative_ontology:affects_constraint(employment_data_extraction, worker_reputation_data_commodification).

% DUAL FORMULATION NOTE:
% Employment data extraction decomposes into three structurally distinct constraints: (1) hiring data extraction (applicant perspective; focused on access control), (2) workplace surveillance (worker perspective; focused on ongoing monitoring), (3) post-employment data retention and commodification (worker privacy commons perspective; focused on lifetime record-keeping). Each has its own epsilon value and temporal dynamics. This story captures the integrated constraint across all three phases; the network links identify the decomposed stories that examine each phase in detail.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(employment_data_extraction, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
