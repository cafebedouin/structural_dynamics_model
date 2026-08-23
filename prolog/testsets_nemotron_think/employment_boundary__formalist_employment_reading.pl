% ============================================================================
% CONSTRAINT STORY: employment_boundary__formalist_employment_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_employment_boundary__formalist_employment_reading, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: employment_boundary__formalist_employment_reading
 *   human_readable: Formalist Employment Boundary — Platform Workers as Independent Contractors
 *   domain: labor_economics/platform_economy/social_policy
 *
 * SUMMARY:
 *   This constraint story captures the formalist reading of the employment
 *   boundary kernel: employment status is determined solely by formal
 *   contract and direct human supervision; platform workers who sign
 *   independent contractor agreements and receive algorithmic (not human)
 *   direction are outside the employment relationship. The reading presents
 *   itself as a neutral coordination mechanism (rope) enabling flexible work,
 *   but its operational metrics reveal high extraction (cost externalization
 *   to workers and state systems) and active suppression (algorithmic
 *   control, misclassification enforcement, lobbying against
 *   reclassification). The engine will compute per-seat classifications from
 *   the structural data; the claimed_type reflects the reading's
 *   self-presentation, not the author's structural assessment.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(employment_boundary__formalist_employment_reading, 0.78).
domain_priors:suppression_score(employment_boundary__formalist_employment_reading, 0.72).
domain_priors:theater_ratio(employment_boundary__formalist_employment_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(employment_boundary__formalist_employment_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(employment_boundary__formalist_employment_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(employment_boundary__formalist_employment_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(employment_boundary__formalist_employment_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(employment_boundary__formalist_employment_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(employment_boundary__formalist_employment_reading, rope).
narrative_ontology:human_readable(employment_boundary__formalist_employment_reading, "Formalist Employment Boundary — Platform Workers as Independent Contractors").
narrative_ontology:topic_domain(employment_boundary__formalist_employment_reading, "labor_economics/platform_economy/social_policy").

domain_priors:requires_active_enforcement(employment_boundary__formalist_employment_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(employment_boundary__formalist_employment_reading, 'ce1a8267-c9a9-4d36-92d3-2915a8c74776').
narrative_ontology:cs_kernel_codification('ce1a8267-c9a9-4d36-92d3-2915a8c74776', formalized).
narrative_ontology:cs_authority_grounding('ce1a8267-c9a9-4d36-92d3-2915a8c74776', lineage).
narrative_ontology:cs_interpretation_layer_present('ce1a8267-c9a9-4d36-92d3-2915a8c74776').
narrative_ontology:cs_reading_relation('ce1a8267-c9a9-4d36-92d3-2915a8c74776', employment_boundary__substantive_employment_reading, coexists_with).
narrative_ontology:cs_reading_relation('ce1a8267-c9a9-4d36-92d3-2915a8c74776', employment_boundary__hybrid_security_reading, coexists_with).
narrative_ontology:cs_axiom('ce1a8267-c9a9-4d36-92d3-2915a8c74776', foundational, contractual_form_determines_employment_status).
narrative_ontology:cs_axiom_status(contractual_form_determines_employment_status, holdable).
narrative_ontology:cs_axiom_grounding('ce1a8267-c9a9-4d36-92d3-2915a8c74776', contractual_form_determines_employment_status, conventional).
narrative_ontology:cs_axiom('ce1a8267-c9a9-4d36-92d3-2915a8c74776', secondary, algorithmic_control_insufficient_for_employment).
narrative_ontology:cs_axiom_status(algorithmic_control_insufficient_for_employment, holdable).
narrative_ontology:cs_axiom_grounding('ce1a8267-c9a9-4d36-92d3-2915a8c74776', algorithmic_control_insufficient_for_employment, conventional).
narrative_ontology:cs_reference_frame('ce1a8267-c9a9-4d36-92d3-2915a8c74776', common_law_master_servant_doctrine).
narrative_ontology:cs_drift_state('ce1a8267-c9a9-4d36-92d3-2915a8c74776', platform_economy_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('ce1a8267-c9a9-4d36-92d3-2915a8c74776', '').
narrative_ontology:cs_kernel_id(employment_boundary__formalist_employment_reading, employment_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(employment_boundary__formalist_employment_reading, platform_companies).
narrative_ontology:constraint_beneficiary(employment_boundary__formalist_employment_reading, venture_capital_backers).
narrative_ontology:constraint_victim(employment_boundary__formalist_employment_reading, platform_workers).
narrative_ontology:constraint_victim(employment_boundary__formalist_employment_reading, state_social_insurance_systems).
narrative_ontology:constraint_victim(employment_boundary__formalist_employment_reading, traditional_employers_subject_to_undercutting).
narrative_ontology:constraint_vindicates(employment_boundary__formalist_employment_reading, freedom_of_contract_doctrine).
narrative_ontology:constraint_vindicates(employment_boundary__formalist_employment_reading, entrepreneurial_flexibility_narrative).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design and enforce the contractual classification of workers as independent contractors. Write the terms of service, control algorithmic task assignment and pay rates, and lobby to maintain the formalist boundary. Collect the surplus between what workers would cost as employees (benefits, insurance, overtime, liability) and what they pay as contractors. Exit is trivial — they can reclassify workers if compelled, but fight classification changes jurisdiction by jurisdiction.
narrative_ontology:constraint_stakeholder(employment_boundary__formalist_employment_reading, platform_companies, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(employment_boundary__formalist_employment_reading, platform_companies, beneficiary).

% Fund platform companies on the premise that labor costs scale variably with zero employer-side obligations. The formalist boundary is a core assumption in unit-economics models and valuation. They do not operate platforms but capture returns from the cost-externalization model. Exit is capital reallocation — mobile across sectors and geographies.
narrative_ontology:constraint_stakeholder(employment_boundary__formalist_employment_reading, venture_capital_backers, beneficiary,
    powerful, biographical, arbitrage, global).

% Perform work under algorithmic direction (task assignment, rating, deactivation) without employment protections: no minimum wage floor, no overtime, no unemployment insurance, no workers' compensation, no collective bargaining right, no anti-discrimination coverage beyond platform's voluntary policy. Bear all capital costs (vehicle, equipment, insurance) and platform fees. 'Flexibility' is the stated benefit; in practice, economic dependence on a single platform and algorithmic discipline constrain real autonomy. Exit means losing primary income source and platform-specific capital investment.
narrative_ontology:constraint_stakeholder(employment_boundary__formalist_employment_reading, platform_workers, payer,
    moderate, biographical, constrained, global).

% Absorb the externalized costs when platform workers face injury, illness, unemployment, or retirement without employer contributions. Unemployment insurance, workers' compensation, disability, and pension systems pay out for workers who had no employer paying in. The state cannot exit the obligation to prevent destitution; it can only chase misclassification through enforcement actions that lag years behind platform growth.
narrative_ontology:constraint_stakeholder(employment_boundary__formalist_employment_reading, state_social_insurance_systems, payer,
    institutional, generational, trapped, national).

% Compete with platform labor that carries 20–30% lower per-hour cost because employer-side taxes, benefits, and compliance are shifted to the worker. Cannot match platform pricing without reducing their own workforce standards. Exit means exiting the market or lobbying for classification reform — neither is quick or certain.
narrative_ontology:constraint_stakeholder(employment_boundary__formalist_employment_reading, traditional_employers_subject_to_undercutting, payer,
    organized, biographical, constrained, national).

% Adjudicate misclassification claims case by case under tests that vary by jurisdiction (ABC test, economic realities, control test). Their rulings create a patchwork: some jurisdictions force reclassification, others uphold contractor status. They observe the structural dynamics but lack a unified standard to resolve the boundary at scale.
narrative_ontology:constraint_stakeholder(employment_boundary__formalist_employment_reading, labor_regulators_and_courts, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Platforms match workers to tasks in real time, handle payment processing, and provide a reputation system — solving search and trust problems in fragmented labor markets.
% TRANSFER_FUNCTION: Moves employer-side social insurance contributions, benefits costs, compliance overhead, and capital equipment costs from platform companies to individual workers and state systems; moves the surplus (platform fee + avoided employer costs) to platform companies and their investors.
% ABSENT_VOICES: Workers who would enter platform work but are excluded by algorithmic barriers (rating thresholds, geographic redlining, vehicle requirements) and never appear in the 'flexibility' narrative; informal caregivers (disproportionately women) who absorb the unpaid labor when platform work lacks sick leave or predictable scheduling; taxpayers in jurisdictions where state systems bear the cost but platform revenue is booked offshore.
% DISAPPEARANCE_RATIONALE: If the formalist boundary vanished overnight and platform workers were presumed employees, platforms would face immediate employer obligations (payroll taxes, benefits, wage floors, liability). Business models would restructure: some would convert to W-2 employment with higher prices; some would exit markets; some would automate task distribution further. Workers would gain protections but lose 'flexible' scheduling autonomy. State insurance systems would receive employer contributions. The labor market would reorganize around a new cost structure.
% FOUNDING_PROBLEM: Early digital platforms (c. 2009–2015) needed a legal structure to onboard millions of workers instantly without the administrative burden and fixed costs of traditional employment, enabling rapid scaling of on-demand services.
% FOUNDING_PROBLEM_CORROBORATION: Platform companies and venture capital attest the founding problem is live — they argue the model cannot survive employment classification. Independent economic analyses (e.g., UC Berkeley Labor Center, 2021; ILO 2023) and legislative findings in California (AB5), the EU (Platform Work Directive), and multiple state supreme courts attest the founding problem is substantially solved — the coordination function persists under employment models, and the boundary now primarily serves cost externalization.
narrative_ontology:disappearance_verdict(employment_boundary__formalist_employment_reading, world_rearranges).
narrative_ontology:founding_problem_status(employment_boundary__formalist_employment_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(employment_boundary__formalist_employment_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(employment_boundary__formalist_employment_reading, 'none', 1).
narrative_ontology:epsilon_provenance(employment_boundary__formalist_employment_reading, 0.78, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(employment_boundary__formalist_employment_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(employment_boundary__formalist_employment_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(employment_boundary__formalist_employment_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.78) is high because the gap between employee cost and contractor pay is systematically captured by platforms — employer-side payroll taxes (~7.65% US), workers' comp, unemployment insurance, health benefits, overtime, and capital costs are shifted to workers or the state. Suppression (0.72) reflects algorithmic discipline (deactivation for low acceptance rates, rating systems), contractual barriers to collective action, and jurisdictional forum-shopping by platforms. Theater ratio (0.48) is rising: the 'entrepreneurial flexibility' narrative increasingly covers extraction as platform take-rates increase and worker autonomy decreases. Accessibility collapse (0.58) is moderate — alternatives exist (traditional jobs, other platforms) but are constrained by local labor market conditions and platform-specific capital investment. Resistance (0.55) includes misclassification lawsuits, AB5/Prop 22 battles, EU Directive implementation, and worker organizing (e.g., App Workers United, IDWF).
 *
 * PERSPECTIVAL GAP:
 *   From the platform seat, the arrangement is genuine coordination: they built the matching infrastructure, workers opt in, and flexibility is real. From the worker seat, the same structure operates as enforced extraction: algorithmic control replicates supervision without the legal obligations, and 'choice' is constrained by necessity. From the state seat, it is a fiscal externality: the state insures the risks the platform externalizes. The engine computes this divergence from the declared roles, power, exit, and scope — the authored claim (rope) does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Platform companies and VC backers are structural beneficiaries (d ~ 0.1–0.2): they collect the avoided-employer-cost surplus and control the classification rule. Platform workers are structural targets (d ~ 0.85–0.95): they bear the transferred costs, face algorithmic discipline, and have constrained exit (invested capital, economic dependence, lack of portable benefits). State insurance systems are trapped targets (d ~ 0.95): they cannot exit the social obligation and lack direct leverage over platform classification. Traditional employers are constrained payers (d ~ 0.7): they bear competitive disadvantage but can lobby. Regulators/courts are analytical observers (d = 0.5).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (rapid onboarding without employment administration) was real in 2010–2015. By 2020, platforms had matured administrative capacity (payroll systems, compliance teams for other jurisdictions) and the coordination function no longer required the formalist boundary — witness Uber's UK worker reclassification post-Supreme Court ruling with continued operation. The boundary persists because it extracts ~$4–8B/year in avoided employer costs in the US alone (NELP 2022). Mandatrophy is unresolved: the arrangement's original justification has atrophied while extraction intensified.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Is the formalist reading a genuine coordination mechanism or a constructed boundary that extracts from workers by denying them the protections the coordination function does not require?',
    'Compare platform labor outcomes in jurisdictions that adopted substantive tests (UK post-Uber BV v Aslam, California AB5 sectors) vs. those retaining formalist tests: if coordination (matching, payment, reputation) persists without the formalist boundary, the boundary is extractive cover.',
    'If coordination persists without the boundary, the formalist reading is a snare/tangled_rope masquerading as rope; if coordination collapses, the boundary has genuine coordination necessity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, empirical, 'Whether the formalist employment boundary is structurally necessary for platform coordination or an extractive overlay.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (algorithmic control, economic dependence, legal barriers to reclassification) or internalized (workers identify as entrepreneurs, believe flexibility outweighs protections)?',
    'Post-exit trajectory study: track workers who leave platform work — do they report feeling coerced during engagement, or do they retrospectively endorse the arrangement? Compare with workers in jurisdictions where reclassification occurred.',
    'If internalized suppression is significant, the constraint''s effective suppression is higher than structural measures suggest — workers carry the constraint''s discipline after exit, reducing resistance and delaying political mobilization.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, conceptual, 'Structural vs. internalized suppression in platform worker self-conception.').

omega_variable(
    coordination_extraction_separability,
    'Can the platform''s coordination function (matching, payment, reputation) be separated from the formalist employment boundary, or does the coordination inherently require contractor classification?',
    'Natural experiment from EU Platform Work Directive implementation (2024–2026): if platforms maintain matching quality and worker onboarding speed under presumed-employment rules, the functions are separable.',
    'If separable, the formalist boundary is pure extraction riding a real coordination function (tangled_rope); if inseparable, part of measured extraction is the price of coordination itself (rope with high inherent cost).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_separability, empirical, 'Whether platform coordination and contractor classification are structurally separable.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(employment_boundary__formalist_employment_reading, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(employment_boundary_formalist_tr_t0, employment_boundary__formalist_employment_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(employment_boundary_formalist_tr_t3, employment_boundary__formalist_employment_reading, theater_ratio, 3, 0.32).
narrative_ontology:measurement(employment_boundary_formalist_tr_t6, employment_boundary__formalist_employment_reading, theater_ratio, 6, 0.38).
narrative_ontology:measurement(employment_boundary_formalist_tr_t9, employment_boundary__formalist_employment_reading, theater_ratio, 9, 0.43).
narrative_ontology:measurement(employment_boundary_formalist_tr_t12, employment_boundary__formalist_employment_reading, theater_ratio, 12, 0.46).
narrative_ontology:measurement(employment_boundary_formalist_tr_t15, employment_boundary__formalist_employment_reading, theater_ratio, 15, 0.48).

% Extraction over time
narrative_ontology:measurement(employment_boundary_formalist_be_t0, employment_boundary__formalist_employment_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(employment_boundary_formalist_be_t3, employment_boundary__formalist_employment_reading, base_extractiveness, 3, 0.55).
narrative_ontology:measurement(employment_boundary_formalist_be_t6, employment_boundary__formalist_employment_reading, base_extractiveness, 6, 0.63).
narrative_ontology:measurement(employment_boundary_formalist_be_t9, employment_boundary__formalist_employment_reading, base_extractiveness, 9, 0.71).
narrative_ontology:measurement(employment_boundary_formalist_be_t12, employment_boundary__formalist_employment_reading, base_extractiveness, 12, 0.76).
narrative_ontology:measurement(employment_boundary_formalist_be_t15, employment_boundary__formalist_employment_reading, base_extractiveness, 15, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(employment_boundary_formalist_su_t0, employment_boundary__formalist_employment_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(employment_boundary_formalist_su_t3, employment_boundary__formalist_employment_reading, suppression_requirement, 3, 0.55).
narrative_ontology:measurement(employment_boundary_formalist_su_t6, employment_boundary__formalist_employment_reading, suppression_requirement, 6, 0.62).
narrative_ontology:measurement(employment_boundary_formalist_su_t9, employment_boundary__formalist_employment_reading, suppression_requirement, 9, 0.68).
narrative_ontology:measurement(employment_boundary_formalist_su_t12, employment_boundary__formalist_employment_reading, suppression_requirement, 12, 0.7).
narrative_ontology:measurement(employment_boundary_formalist_su_t15, employment_boundary__formalist_employment_reading, suppression_requirement, 15, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(employment_boundary__formalist_employment_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(employment_boundary__formalist_employment_reading, 0.15).
narrative_ontology:affects_constraint(employment_boundary__formalist_employment_reading, employment_boundary__substantive_employment_reading).
narrative_ontology:affects_constraint(employment_boundary__formalist_employment_reading, employment_boundary__hybrid_security_reading).
narrative_ontology:affects_constraint(employment_boundary__formalist_employment_reading, platform_commission_gatekeeping).
narrative_ontology:affects_constraint(employment_boundary__formalist_employment_reading, algorithmic_management_discipline).
narrative_ontology:affects_constraint(employment_boundary__formalist_employment_reading, social_insurance_financing_gap).

% DUAL FORMULATION NOTE:
% This constraint is one member of the employment_boundary constraint family (kernel_id: employment_boundary). The formalist reading defines employment by formal contract + direct supervision; the substantive reading defines it by economic dependence + algorithmic control; the hybrid reading proposes a third category. Their ε values differ substantially: formalist reading ε ≈ 0.78 (high extraction via exclusion), substantive reading ε ≈ 0.35 (employer obligations internalized), hybrid reading ε ≈ 0.5 (partial protections). They are linked via network.affects_constraints to enable contamination analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(employment_boundary__formalist_employment_reading, institutional, 0.15).
constraint_indexing:directionality_override(employment_boundary__formalist_employment_reading, moderate, 0.88).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
