% ============================================================================
% CONSTRAINT STORY: categorical_violence_as_structural_exclusion
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_categorical_violence_as_structural_exclusion, []).

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
 *   constraint_id: categorical_violence_as_structural_exclusion
 *   human_readable: Categorical Violence as Structural Exclusion in Medical Authority Systems
 *   domain: institutional_violence/medical_authority/labor_extraction
 *
 * SUMMARY:
 *   Administrative categories in medical authority systems create structural
 *   exclusions that render certain subjects ineligible for standard
 *   protections and care protocols. These categories — immigration status,
 *   insurance coverage tier, diagnostic code eligibility, facility admission
 *   criteria — serve genuine coordination functions (resource allocation,
 *   liability management, treatment standardization) while simultaneously
 *   creating zones of exception where institutional violence becomes
 *   permissible. The constraint exhibits a perspectival gap between
 *   institutional actors who experience the categories as pure coordination
 *   (rope) and excluded subjects who experience them as extraction mechanisms
 *   (snare). Frontline medical workers occupy an intermediate position,
 *   benefiting from decision-rule clarity while bearing the moral injury of
 *   enforcing exclusions. The theater_ratio (0.58) reflects that much
 *   administrative labor goes to category maintenance, exception processing,
 *   and appeals documentation rather than direct care coordination. The
 *   constraint's extractiveness has increased over the interval (0.28 → 0.38)
 *   as cost pressures have tightened category boundaries and reduced
 *   exception accessibility.
 *
 * KEY AGENTS:
 *   - Categorically Excluded Subjects: Primary victims (powerless/trapped) — rendered ineligible for standard protections by administrative classification; cannot exit category without institutional reclassification
 *   - Frontline Medical Workers: Secondary victims (moderate/constrained) — forced to enforce exclusions; bear moral injury and emotional labor; benefit from decision-rule clarity
 *   - Protocol Designers: Primary beneficiaries (institutional/arbitrage) — institutional actors who design categorical systems; experience categories as pure coordination solving resource allocation problems
 *   - Patient Advocacy Coalitions: Organized agents (organized/mobile) — work to expand category definitions; see both coordination function and extraction; moderate effective extraction due to agency
 *   - Insurance Bureaucracy: Institutional actor (institutional/constrained) — maintains categorical systems through inertia; high theater ratio; recognizes own performative nature but cannot exit
 *   - Insured Patient Class: Indirect beneficiaries (moderate/mobile) — benefit from categorical exclusions that concentrate resources on insured populations
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees genuine coordination function but risks naturalizing the violence of categorical exclusion
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(categorical_violence_as_structural_exclusion, 0.38).
domain_priors:suppression_score(categorical_violence_as_structural_exclusion, 0.62).
domain_priors:theater_ratio(categorical_violence_as_structural_exclusion, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(categorical_violence_as_structural_exclusion, extractiveness, 0.38).
narrative_ontology:constraint_metric(categorical_violence_as_structural_exclusion, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(categorical_violence_as_structural_exclusion, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(categorical_violence_as_structural_exclusion, rope).
narrative_ontology:human_readable(categorical_violence_as_structural_exclusion, "Categorical Violence as Structural Exclusion in Medical Authority Systems").
narrative_ontology:topic_domain(categorical_violence_as_structural_exclusion, "institutional_violence/medical_authority/labor_extraction").

domain_priors:requires_active_enforcement(categorical_violence_as_structural_exclusion).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(categorical_violence_as_structural_exclusion, institutional_administrators).
narrative_ontology:constraint_beneficiary(categorical_violence_as_structural_exclusion, protocol_designers).
narrative_ontology:constraint_beneficiary(categorical_violence_as_structural_exclusion, insured_patient_class).
narrative_ontology:constraint_victim(categorical_violence_as_structural_exclusion, categorically_excluded_subjects).
narrative_ontology:constraint_victim(categorical_violence_as_structural_exclusion, frontline_medical_workers).
narrative_ontology:constraint_victim(categorical_violence_as_structural_exclusion, uninsured_patient_class).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CATEGORICALLY EXCLUDED SUBJECT (SNARE) — Trapped by administrative classification that renders them ineligible for standard protections. Cannot exit the category without institutional reclassification. Bears full extraction: denied care, excluded from protocols, rendered legally invisible. The category itself is the cage.
constraint_indexing:constraint_classification(categorical_violence_as_structural_exclusion, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: FRONTLINE MEDICAL WORKER (TANGLED ROPE) — Constrained by protocol requirements that force differential treatment. Benefits from clear decision rules that reduce liability and cognitive load, but also bears moral injury from enforcing exclusions. Mixed experience: the categories coordinate triage decisions while extracting emotional labor and ethical compromise.
constraint_indexing:constraint_classification(categorical_violence_as_structural_exclusion, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: PROTOCOL DESIGNER (ROPE) — Institutional actor with arbitrage exit. Experiences categorical systems as pure coordination: administrative categories enable resource allocation, liability management, and operational efficiency. Net beneficiary — the categories solve genuine coordination problems (who gets scarce resources, who is responsible for outcomes) with minimal perceived extraction.
constraint_indexing:constraint_classification(categorical_violence_as_structural_exclusion, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: PATIENT ADVOCACY COALITION (TANGLED ROPE) — Organized agents working to expand category definitions and challenge exclusions. Mobile exit (can shift advocacy focus to other issues) but constrained by institutional resistance. See both coordination function (categories enable systematic care delivery) and extraction (arbitrary boundaries exclude vulnerable populations). Moderate effective extraction because coalition has agency and some success in category expansion.
constraint_indexing:constraint_classification(categorical_violence_as_structural_exclusion, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: INSURANCE BUREAUCRACY (PITON) — Institutional actor that maintains categorical systems through inertia rather than function. High theater ratio: much administrative labor goes to category maintenance, appeals processing, and exception documentation rather than actual care coordination. The bureaucracy recognizes its own performative nature but cannot exit due to path dependence and sunk institutional investment.
constraint_indexing:constraint_classification(categorical_violence_as_structural_exclusion, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (ROPE) — From a civilizational analytical perspective, administrative categories in medical systems serve genuine coordination functions: they enable resource allocation, standardize treatment protocols, and manage institutional liability. The constraint is classified as rope because the coordination function is real and the base extractiveness (0.38) falls within rope range when scaled by analytical context parameters. However, the perspectival gap reveals that this coordination comes at significant cost to those excluded by the categories.
constraint_indexing:constraint_classification(categorical_violence_as_structural_exclusion, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(categorical_violence_as_structural_exclusion_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(categorical_violence_as_structural_exclusion, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(categorical_violence_as_structural_exclusion, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(categorical_violence_as_structural_exclusion, TR),
    TR >= 0.70.

:- end_tests(categorical_violence_as_structural_exclusion_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. Administrative categories extract from excluded subjects through denial of care and legal invisibility, and from frontline workers through moral injury and emotional labor. However, the categories also serve genuine coordination functions — they enable systematic resource allocation and standardize treatment protocols. The value reflects that the extraction is real but not total; some coordination benefit exists. Suppression (0.62): Moderate-high. Excluded subjects face significant barriers to reclassification: bureaucratic complexity, documentation requirements, legal status constraints, and institutional resistance to exception-making. Frontline workers face professional and legal constraints on deviating from categorical protocols. But suppression is not total — advocacy coalitions achieve some category expansions, and exception processes (though often performative) do occasionally succeed. Theater ratio (0.58): Moderate-high. Much administrative labor in categorical systems is performative: appeals processing that rarely succeeds, exception documentation that serves liability management rather than care access, category maintenance rituals that persist through inertia. The theater has increased over the interval as cost pressures have added layers of bureaucratic justification without expanding actual care access.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates a diagnostic perspectival gap between institutional actors and excluded subjects. Protocol designers and insured patients experience categorical systems as pure coordination (rope) — the categories solve genuine problems of resource allocation and treatment standardization. Frontline medical workers experience mixed coordination and extraction (tangled_rope) — the categories provide decision-rule clarity while forcing them to enforce exclusions that cause moral injury. Categorically excluded subjects experience pure extraction (snare) — the categories render them legally invisible and ineligible for care, with no exit option except institutional reclassification they cannot control. The analytical observer's rope classification reflects the genuine coordination function but risks naturalizing the violence of categorical exclusion as an inevitable feature of medical systems. The gap reveals that 'coordination' and 'extraction' are not mutually exclusive — the same categorical system coordinates care for included populations while extracting from excluded populations.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from structural position and beneficiary/victim declarations. Categorically excluded subjects are victims with trapped exit options, yielding high d and high experienced extraction (snare classification). Protocol designers are beneficiaries with arbitrage exit, yielding low d and low/negative experienced extraction (rope classification). Frontline medical workers are victims (bear moral injury) but with constrained rather than trapped exit and some benefit from decision-rule clarity, yielding moderate d and moderate experienced extraction (tangled_rope classification). Patient advocacy coalitions are organized agents with mobile exit working to expand categories, yielding moderate d despite their advocacy role. The insurance bureaucracy is an institutional actor with constrained exit (path dependence) rather than arbitrage, yielding moderate d and piton classification driven by theater gate rather than high chi. The analytical observer uses canonical analytical d, yielding rope classification that reflects the genuine coordination function while the perspectival gap reveals the cost to excluded subjects.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy by demonstrating that categorical systems in medical authority are simultaneously coordination mechanisms and extraction mechanisms, depending on structural position. The rope classification from institutional and analytical perspectives is not false — the categories do solve genuine coordination problems. The snare classification from excluded subjects' perspective is also not false — the categories do create zones of exception where institutional violence becomes permissible. The tangled_rope classification from frontline workers' perspective captures the mixed experience of benefiting from decision-rule clarity while bearing the cost of enforcing exclusions. The mandatrophy resolution is that all three classifications are legitimate perspectival readings of the same structural data. The presheaf over observation sites IS the answer: the constraint is rope from positions of inclusion, snare from positions of exclusion, and tangled_rope from positions of enforcement. The analytical challenge is not to choose one classification but to map the perspectival structure that produces different classifications from different positions.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    category_boundary_arbitrariness,
    'Are the boundaries of exclusionary categories determined by medical necessity or by administrative convenience and cost containment?',
    'Comparative analysis of category definitions across jurisdictions with different cost structures; correlation between category boundaries and institutional budget constraints vs clinical outcomes',
    'If medically necessary: classification shifts toward rope (genuine coordination). If administratively convenient: classification shifts toward snare (extraction disguised as coordination).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(category_boundary_arbitrariness, empirical, 'Whether category boundaries reflect medical necessity or administrative cost containment').

omega_variable(
    exception_clause_accessibility,
    'Do exception clauses and appeals processes provide genuine recourse or performative bureaucratic theater?',
    'Success rate analysis of appeals by category; time-to-resolution vs time-to-medical-necessity; correlation between appeal success and patient resources (legal representation, documentation capacity)',
    'If accessible: suppression is lower than measured, classification shifts toward rope. If performative: suppression is higher, classification shifts toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exception_clause_accessibility, empirical, 'Whether exception processes provide genuine recourse or theater').

omega_variable(
    frontline_worker_moral_injury,
    'What proportion of frontline medical worker burnout and attrition is attributable to enforcing categorical exclusions vs other job stressors?',
    'Longitudinal studies of worker retention and mental health outcomes; qualitative analysis of exit interviews; comparison of burnout rates in systems with vs without categorical exclusions',
    'If significant: extraction from frontline workers is higher than base metric suggests, strengthening tangled_rope classification. If minimal: extraction is primarily on excluded subjects, not workers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(frontline_worker_moral_injury, empirical, 'Proportion of worker burnout attributable to categorical enforcement').

omega_variable(
    category_expansion_trajectory,
    'Are categorical boundaries expanding to include previously excluded populations, or contracting under cost pressure?',
    'Historical analysis of category definition changes over time; correlation with institutional budget cycles and political pressure; tracking of advocacy coalition success rates',
    'If expanding: constraint has scaffold properties (sunset logic). If contracting: extraction is increasing, strengthening snare classification from excluded subjects'' perspective.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(category_expansion_trajectory, empirical, 'Trajectory of category boundary evolution over time').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(categorical_violence_as_structural_exclusion, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(catviolence_tr_t0, categorical_violence_as_structural_exclusion, theater_ratio, 0, 0.42).
narrative_ontology:measurement(catviolence_tr_t3, categorical_violence_as_structural_exclusion, theater_ratio, 3, 0.48).
narrative_ontology:measurement(catviolence_tr_t6, categorical_violence_as_structural_exclusion, theater_ratio, 6, 0.53).
narrative_ontology:measurement(catviolence_tr_t10, categorical_violence_as_structural_exclusion, theater_ratio, 10, 0.58).

% Extraction over time
narrative_ontology:measurement(catviolence_be_t0, categorical_violence_as_structural_exclusion, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(catviolence_be_t3, categorical_violence_as_structural_exclusion, base_extractiveness, 3, 0.32).
narrative_ontology:measurement(catviolence_be_t6, categorical_violence_as_structural_exclusion, base_extractiveness, 6, 0.35).
narrative_ontology:measurement(catviolence_be_t10, categorical_violence_as_structural_exclusion, base_extractiveness, 10, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(categorical_violence_as_structural_exclusion, resource_allocation).

% DUAL FORMULATION NOTE:
% This constraint is part of a broader family of institutional violence mechanisms. Related constraints include: (1) diagnostic_code_gatekeeping (categorical exclusion via medical classification systems), (2) immigration_status_care_denial (categorical exclusion via legal status), (3) insurance_tier_treatment_differential (categorical exclusion via payment classification). Each has distinct ε values reflecting different extraction mechanisms, but all share the structural pattern of administrative categories creating zones of exception.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(categorical_violence_as_structural_exclusion, institutional, 0.48).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
