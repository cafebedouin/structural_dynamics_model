% ============================================================================
% CONSTRAINT STORY: triadic_care_arbitrage
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-02
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_triadic_care_arbitrage, []).

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
 *   constraint_id: triadic_care_arbitrage
 *   human_readable: Triadic Care Model: AI-Mediated Physician Authority Extension
 *   domain: healthcare_ai/clinical_practice/technology_governance
 *
 * SUMMARY:
 *   The triadic care model emerged as a response to projected healthcare
 *   workforce shortages: 10M physician shortfall globally by 2030, with
 *   particularly acute gaps in primary care and rural medicine. The model
 *   positions AI as an intermediary layer that extends physician supervisory
 *   capacity over non-physician healthcare workers (nurse practitioners,
 *   physician assistants, community health workers), enabling one physician
 *   to oversee 3-5x more patient encounters than traditional practice models.
 *   This structure solves a genuine coordination problem (workforce shortage)
 *   while embedding extraction through three mechanisms: (1) deskilling of
 *   non-physician workers whose clinical judgment is subordinated to
 *   algorithmic protocols, (2) concentration of economic rents with physician
 *   and platform layers despite workers bearing increased patient load, and
 *   (3) regulatory capture that maintains physician supervision requirements
 *   even as AI diagnostic capabilities mature. The constraint exhibits rising
 *   theater_ratio (0.35 → 0.58) as physician 'supervision' becomes
 *   increasingly pro forma — reviewing AI-flagged cases rather than providing
 *   substantive clinical oversight — and rising extractiveness (0.32 → 0.48)
 *   as the model's efficiency gains accrue to systems and physicians while
 *   workers experience wage compression and autonomy loss. The model is
 *   presented as inevitable technological progress ('AI augmentation') but
 *   the triadic topology is a design choice that preserves existing
 *   professional hierarchies rather than the only possible configuration.
 *
 * KEY AGENTS:
 *   - Non-Physician Healthcare Workers: Primary victim (powerless/trapped) — nurse practitioners, physician assistants, community health workers bearing deskilling, increased monitoring, wage compression, and liability without authority
 *   - Healthcare System Administrators: Primary beneficiary (institutional/arbitrage) — solve workforce shortage while reducing labor costs per patient encounter; can arbitrage between competing AI platforms
 *   - Physician Professional Associations: Secondary beneficiary (institutional/arbitrage) — preserve physician authority topology and extend economic reach through supervision of larger patient panels
 *   - AI Platform Vendors: Secondary beneficiary (institutional/arbitrage) — capture recurring revenue from supervision infrastructure; benefit from regulatory requirements that mandate platform intermediation
 *   - Independent Practice Clinicians: Mixed position (moderate/constrained) — benefit from efficiency gains but face platform lock-in and loss of practice autonomy
 *   - Healthcare Worker Union Coalition: Organized resistance (organized/mobile) — building alternative governance models and advocating for scope-of-practice reform to eliminate physician bottleneck
 *   - Patient Continuity of Care: Abstract victim (powerless/trapped) — fragmentation of care relationships as workers rotate through AI-supervised encounters rather than maintaining longitudinal patient relationships
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(triadic_care_arbitrage, 0.48).
domain_priors:suppression_score(triadic_care_arbitrage, 0.62).
domain_priors:theater_ratio(triadic_care_arbitrage, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(triadic_care_arbitrage, extractiveness, 0.48).
narrative_ontology:constraint_metric(triadic_care_arbitrage, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(triadic_care_arbitrage, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(triadic_care_arbitrage, tangled_rope).
narrative_ontology:human_readable(triadic_care_arbitrage, "Triadic Care Model: AI-Mediated Physician Authority Extension").
narrative_ontology:topic_domain(triadic_care_arbitrage, "healthcare_ai/clinical_practice/technology_governance").

domain_priors:requires_active_enforcement(triadic_care_arbitrage).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(triadic_care_arbitrage, healthcare_systems_facing_workforce_shortage).
narrative_ontology:constraint_beneficiary(triadic_care_arbitrage, physician_groups).
narrative_ontology:constraint_beneficiary(triadic_care_arbitrage, ai_platform_vendors).
narrative_ontology:constraint_victim(triadic_care_arbitrage, non_physician_healthcare_workers).
narrative_ontology:constraint_victim(triadic_care_arbitrage, patient_continuity_of_care).
narrative_ontology:constraint_victim(triadic_care_arbitrage, independent_practice_clinicians).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NON-PHYSICIAN HEALTHCARE WORKER (SNARE) — Trapped in a deskilling trajectory where AI supervision replaces autonomous clinical judgment. Cannot exit without abandoning healthcare career entirely. Bears full extraction: increased workload monitoring, reduced professional autonomy, wage compression justified by 'AI assistance,' and liability risk without corresponding authority. The triadic model positions these workers as interchangeable execution layers rather than independent practitioners.
constraint_indexing:constraint_classification(triadic_care_arbitrage, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: INDEPENDENT PRACTICE CLINICIAN (TANGLED ROPE) — Constrained by market pressure to adopt triadic models or lose competitive position, but also benefits from genuine efficiency gains in routine case management. Experiences mixed extraction: the model solves real coordination problems (extending reach, managing panel size) while simultaneously eroding practice autonomy through platform lock-in and algorithmic supervision requirements. Can exit by selling practice or retiring, but at significant career cost.
constraint_indexing:constraint_classification(triadic_care_arbitrage, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: HEALTHCARE SYSTEM ADMINISTRATOR (ROPE) — Primary beneficiary experiencing the constraint as pure coordination. The triadic model solves the genuine problem of 10M projected physician shortfall by 2030 while maintaining regulatory compliance with physician supervision requirements. Arbitrage position: can choose between competing AI platforms, negotiate contracts, and exit underperforming arrangements. Extraction flows toward this agent through labor cost reduction and increased patient throughput.
constraint_indexing:constraint_classification(triadic_care_arbitrage, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: PHYSICIAN PROFESSIONAL ASSOCIATION (ROPE) — Benefits from the model's preservation of physician authority topology while extending economic reach. The triadic structure maintains the regulatory requirement for physician supervision, protecting physician market position while enabling supervision of 3-5x more patient encounters through AI intermediation. Arbitrage exit: can negotiate scope-of-practice regulations, influence licensing requirements, and shape platform governance standards.
constraint_indexing:constraint_classification(triadic_care_arbitrage, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: HEALTHCARE WORKER UNION COALITION (SCAFFOLD) — Organized labor sees the triadic model as a temporary coordination mechanism with a sunset: as AI diagnostic capabilities mature and regulatory frameworks evolve, the physician supervision requirement will become vestigial, enabling direct AI-to-worker or AI-to-patient pathways that bypass the current extraction topology. Coalition is building alternative governance models (worker-owned platforms, professional certification for AI-augmented practice) that would eliminate the physician bottleneck. Estimated sunset: 15-25 years as scope-of-practice laws and AI reliability converge.
constraint_indexing:constraint_classification(triadic_care_arbitrage, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational perspective, the triadic model exhibits genuine coordination function (addressing workforce shortage, maintaining quality gates) alongside structural extraction (deskilling non-physician workers, concentrating economic rents with physician and platform layers, creating artificial bottlenecks through regulatory capture). The model is neither pure coordination nor pure extraction but a hybrid that solves one collective action problem (workforce shortage) while creating another (professional hierarchy lock-in). The analytical classification anchors the constraint's type.
constraint_indexing:constraint_classification(triadic_care_arbitrage, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(triadic_care_arbitrage_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(triadic_care_arbitrage, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(triadic_care_arbitrage, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(triadic_care_arbitrage, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(triadic_care_arbitrage_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48): Moderate-high. The triadic model creates genuine efficiency gains (addressing workforce shortage, enabling rural access) but extraction is substantial and rising. Non-physician workers bear increased patient loads with reduced autonomy and wage compression justified by 'AI assistance.' Physicians capture supervision fees for pro forma oversight. Platforms extract recurring revenue. The efficiency gains are real but asymmetrically distributed — systems and physicians benefit while workers are deskilled. The value reflects that coordination function exists but is increasingly dominated by extractive rent-seeking. Suppression (0.62): High. Multiple mechanisms suppress alternatives: (1) regulatory capture maintains physician supervision requirements even as AI capabilities mature, (2) platform lock-in creates switching costs for systems that adopt triadic models, (3) professional licensing requirements prevent non-physician workers from autonomous practice even when AI-augmented, (4) liability frameworks assign risk to workers without corresponding authority, (5) market concentration among AI vendors limits competitive pressure. Workers cannot exit without abandoning healthcare careers; systems face regulatory barriers to direct AI-to-worker models. Theater ratio (0.58): Moderate-high and rising. Physician supervision is increasingly performative: reviewing AI-flagged cases rather than providing substantive clinical oversight, signing off on algorithmic recommendations without independent assessment, maintaining supervision relationships for regulatory compliance rather than quality improvement. The theater has increased as AI diagnostic capabilities have improved — the physician layer adds less clinical value but remains structurally required. Initial theater (0.35) reflected genuine supervisory function; current theater (0.58) reflects regulatory inertia and professional capture.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates classic beneficiary-victim perspectival divergence. Healthcare system administrators experience pure coordination (rope) — the triadic model solves their workforce shortage problem with minimal extraction from their position. Physician associations also see coordination (rope) — the model extends their economic reach while preserving professional authority. Non-physician workers experience pure extraction (snare) — they are trapped in a deskilling trajectory with no exit option and bear all costs of the model's efficiency gains. Independent clinicians experience mixed coordination and extraction (tangled rope) — genuine efficiency benefits alongside platform lock-in and autonomy loss. The union coalition sees a temporary problem with a sunset (scaffold) — current extraction is real but alternative governance models and regulatory evolution will eventually eliminate the physician bottleneck. The analytical observer sees the hybrid structure (tangled rope) — genuine coordination function addressing workforce shortage alongside structural extraction through professional hierarchy preservation. The gap between system administrator (rope) and worker (snare) perspectives is the diagnostic signature of asymmetric extraction: the same structural arrangement appears as pure coordination from above and pure extraction from below. The scaffold perspective introduces temporal dimension: what appears as permanent extraction (snare) from the worker's biographical horizon appears as temporary coordination failure (scaffold) from the organized coalition's generational horizon.
 *
 * DIRECTIONALITY LOGIC:
 *   Healthcare system administrators are primary beneficiaries with arbitrage exit options — they solve workforce shortage while reducing labor costs and can switch between AI platforms. Directionality derived from beneficiary status + arbitrage exit yields low d → negative or low χ (rope classification). Physician professional associations are secondary beneficiaries with arbitrage exit — they preserve authority topology and extend economic reach through supervision fees. Similar derivation yields rope classification. Non-physician healthcare workers are primary victims with trapped exit options — they cannot leave without abandoning healthcare careers and bear deskilling, wage compression, and increased monitoring. Victim status + trapped exit yields high d → high χ (snare classification). Independent practice clinicians are mixed: they benefit from efficiency gains (secondary beneficiary) but face platform lock-in and autonomy loss (secondary victim). Constrained exit options (can sell practice or retire but at significant cost) combined with mixed beneficiary/victim status yields moderate d → moderate χ (tangled rope classification). The healthcare worker union coalition has organized power and mobile exit options (can build alternative models, advocate for regulatory reform) yielding lower d → lower χ despite being structurally aligned with worker interests (scaffold classification). The analytical observer sees both genuine coordination function and structural extraction, yielding tangled rope classification that anchors the constraint type. AI platform vendors are tertiary beneficiaries (not primary agents in the clinical relationship) with arbitrage exit, yielding rope classification from their perspective (not included as separate perspective to avoid redundancy with system administrators).
 *
 * MANDATROPHY ANALYSIS:
 *   The triadic care model resolves mandatrophy by demonstrating that tangled rope classification requires BOTH genuine coordination function AND asymmetric extraction, with the analytical perspective anchoring the type. The coordination function is real: the model addresses a genuine workforce shortage (10M physician shortfall) and enables healthcare access in underserved areas. This is not theatrical coordination — patients receive care they would not otherwise receive. The extraction is also real: non-physician workers are deskilled, wage-compressed, and subordinated to algorithmic supervision while physicians and systems capture the efficiency gains. The model is neither pure coordination (rope) nor pure extraction (snare) but a hybrid that solves one collective action problem while creating another. The mandatrophy resolution is structural: the constraint exhibits coordination function from beneficiary perspectives (healthcare systems, physician associations) and extraction from victim perspectives (non-physician workers, patient continuity), with the analytical perspective seeing both functions simultaneously. The perspectival gap is not measurement error but the signature of asymmetric distribution of coordination benefits and extraction costs. The tangled rope classification captures this hybrid structure: the model coordinates (addresses workforce shortage) while extracting (deskills workers, concentrates rents, maintains artificial bottlenecks through regulatory capture). The classification prevents both false negatives (missing the genuine coordination function by focusing only on worker deskilling) and false positives (naturalizing the extraction by focusing only on workforce shortage solution).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ai_reliability_threshold,
    'At what diagnostic accuracy threshold does AI supervision become safer than physician supervision for routine cases, making the physician layer extractive rather than coordinative?',
    'Longitudinal outcome studies comparing AI-supervised vs physician-supervised care across matched patient populations; malpractice claim rates; adverse event tracking',
    'If AI reliability exceeds physician reliability for routine cases: physician supervision requirement becomes pure extraction (Snare from more perspectives). If AI remains less reliable: supervision requirement is genuine coordination (Rope from more perspectives).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ai_reliability_threshold, empirical, 'Threshold where AI supervision quality exceeds physician supervision necessity').

omega_variable(
    scope_of_practice_evolution,
    'Will regulatory frameworks evolve to permit direct AI-to-non-physician-worker pathways, or will physician supervision requirements persist through professional capture regardless of AI capability?',
    'Tracking state-level scope-of-practice legislation; analysis of professional lobbying expenditures and regulatory outcomes; international comparison with healthcare systems that permit independent non-physician practice',
    'If regulations evolve with capability: scaffold perspective confirmed, sunset is real. If regulations remain captured: physician layer becomes permanent extraction regardless of AI performance, confirming snare classification from worker perspective.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(scope_of_practice_evolution, preference, 'Whether scope-of-practice laws will adapt to AI capability or remain captured').

omega_variable(
    deskilling_reversibility,
    'Is the deskilling of non-physician workers under AI supervision reversible, or does it create permanent human capital degradation?',
    'Longitudinal studies of clinical judgment retention in AI-supervised vs autonomous practice; comparison of diagnostic accuracy for workers who transition from autonomous to AI-supervised practice; assessment of skill recovery after AI system failure',
    'If reversible: extraction is temporary and workers retain exit option to autonomous practice. If irreversible: extraction is permanent and creates path dependency that locks workers into subordinate roles.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deskilling_reversibility, empirical, 'Whether AI-supervised practice causes permanent clinical skill degradation').

omega_variable(
    platform_governance_capture,
    'Will AI platform governance be captured by physician and health system interests, or will non-physician workers gain meaningful input into algorithmic supervision design?',
    'Analysis of platform governance structures; representation of non-physician workers in algorithm design and audit processes; tracking of algorithmic bias complaints and resolution patterns',
    'If governance remains captured: platform design will optimize for physician authority preservation and system cost reduction at worker expense (Snare). If workers gain governance voice: platforms could evolve toward genuine coordination tools (Rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(platform_governance_capture, preference, 'Whether platform governance will include non-physician worker representation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(triadic_care_arbitrage, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(triadic_theater_t0, triadic_care_arbitrage, theater_ratio, 0, 0.35).
narrative_ontology:measurement(triadic_theater_t3, triadic_care_arbitrage, theater_ratio, 3, 0.45).
narrative_ontology:measurement(triadic_theater_t6, triadic_care_arbitrage, theater_ratio, 6, 0.58).

% Extraction over time
narrative_ontology:measurement(triadic_extract_t0, triadic_care_arbitrage, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(triadic_extract_t3, triadic_care_arbitrage, base_extractiveness, 3, 0.4).
narrative_ontology:measurement(triadic_extract_t6, triadic_care_arbitrage, base_extractiveness, 6, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(triadic_care_arbitrage, resource_allocation).
narrative_ontology:affects_constraint(triadic_care_arbitrage, clinical_authority_topology).
narrative_ontology:affects_constraint(triadic_care_arbitrage, performance_measurement_asymmetry).

% DUAL FORMULATION NOTE:
% The triadic care model is downstream of clinical_authority_topology (the regulatory requirement for physician supervision) and performance_measurement_asymmetry (the differential measurement of AI vs human clinical performance). The upstream constraints shape the triadic model's structure: clinical_authority_topology creates the physician bottleneck that the triadic model exploits, while performance_measurement_asymmetry enables the model to claim coordination benefits (AI improves outcomes) while obscuring extraction costs (worker deskilling, patient continuity loss). Each constraint has its own extractiveness value: clinical_authority_topology reflects the physician supervision requirement's inherent extraction, performance_measurement_asymmetry reflects the measurement framework's bias toward quantifiable outcomes, and triadic_care_arbitrage reflects the specific extraction topology created by AI-mediated supervision.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
