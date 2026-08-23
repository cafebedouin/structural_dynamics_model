% ============================================================================
% CONSTRAINT STORY: employment_boundary__hybrid_security_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_employment_boundary__hybrid_security_reading, []).

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
 *   constraint_id: employment_boundary__hybrid_security_reading
 *   human_readable: Hybrid Third-Category Labor Classification for Platform Workers
 *   domain: labor_economics/platform_economy/social_policy
 *
 * SUMMARY:
 *   The hybrid security reading instantiates a third labor category for
 *   platform workers: not employees, not independent contractors, but a
 *   tailored status with injury insurance (86.2% coverage) and medical access
 *   (91.5%) while excluding retirement, career development, paid leave, and
 *   collective bargaining rights. This constraint is the
 *   legislative/regulatory regime itself (e.g., UK worker status, California
 *   Prop 22, EU Platform Work Directive compromises). It coordinates by
 *   providing a floor of protections where none existed, but extracts by
 *   institutionalizing the retirement/career gap and cementing platform
 *   control over algorithmic management. The claimed_type is tangled_rope:
 *   genuine coordination function (basic protections) + asymmetric extraction
 *   (platforms avoid ~30-40% employment cost while workers bear generational
 *   precarity) + active enforcement (legislation, regulatory certification,
 *   platform compliance monitoring).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(employment_boundary__hybrid_security_reading, 0.52).
domain_priors:suppression_score(employment_boundary__hybrid_security_reading, 0.45).
domain_priors:theater_ratio(employment_boundary__hybrid_security_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(employment_boundary__hybrid_security_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(employment_boundary__hybrid_security_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(employment_boundary__hybrid_security_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(employment_boundary__hybrid_security_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(employment_boundary__hybrid_security_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(employment_boundary__hybrid_security_reading, tangled_rope).
narrative_ontology:human_readable(employment_boundary__hybrid_security_reading, "Hybrid Third-Category Labor Classification for Platform Workers").
narrative_ontology:topic_domain(employment_boundary__hybrid_security_reading, "labor_economics/platform_economy/social_policy").

domain_priors:requires_active_enforcement(employment_boundary__hybrid_security_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(employment_boundary__hybrid_security_reading, 'ef30196e-cf32-4f11-8d50-0630ad37a36f').
narrative_ontology:cs_kernel_codification('ef30196e-cf32-4f11-8d50-0630ad37a36f', distributed).
narrative_ontology:cs_authority_grounding('ef30196e-cf32-4f11-8d50-0630ad37a36f', extraction).
narrative_ontology:cs_interpretation_layer_present('ef30196e-cf32-4f11-8d50-0630ad37a36f').
narrative_ontology:cs_reading_relation('ef30196e-cf32-4f11-8d50-0630ad37a36f', employment_boundary__formalist_employment_reading, coexists_with).
narrative_ontology:cs_reading_relation('ef30196e-cf32-4f11-8d50-0630ad37a36f', employment_boundary__substantive_employment_reading, influences).
narrative_ontology:cs_axiom('ef30196e-cf32-4f11-8d50-0630ad37a36f', foundational, platform_work_requires_distinct_regulatory_category).
narrative_ontology:cs_axiom_status(platform_work_requires_distinct_regulatory_category, holdable).
narrative_ontology:cs_axiom_grounding('ef30196e-cf32-4f11-8d50-0630ad37a36f', platform_work_requires_distinct_regulatory_category, instrumental).
narrative_ontology:cs_axiom('ef30196e-cf32-4f11-8d50-0630ad37a36f', secondary, basic_protections_without_full_employment_is_welfare_improving).
narrative_ontology:cs_axiom_status(basic_protections_without_full_employment_is_welfare_improving, holdable).
narrative_ontology:cs_axiom_grounding('ef30196e-cf32-4f11-8d50-0630ad37a36f', basic_protections_without_full_employment_is_welfare_improving, instrumental).
narrative_ontology:cs_reference_frame('ef30196e-cf32-4f11-8d50-0630ad37a36f', pre_platform_labor_law_binary).
narrative_ontology:cs_drift_state('ef30196e-cf32-4f11-8d50-0630ad37a36f', post_platform_work_directive_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('ef30196e-cf32-4f11-8d50-0630ad37a36f', '').
narrative_ontology:cs_kernel_id(employment_boundary__hybrid_security_reading, employment_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(employment_boundary__hybrid_security_reading, platform_companies).
narrative_ontology:constraint_beneficiary(employment_boundary__hybrid_security_reading, flexibility_preferring_workers).
narrative_ontology:constraint_victim(employment_boundary__hybrid_security_reading, platform_workers_career_precarious).
narrative_ontology:constraint_victim(employment_boundary__hybrid_security_reading, platform_workers_retirement_insecure).
narrative_ontology:constraint_vindicates(employment_boundary__hybrid_security_reading, labor_classification_flexibility_doctrine).
narrative_ontology:constraint_vindicates(employment_boundary__hybrid_security_reading, algorithmic_management_novelty_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design and enforce algorithmic management systems that classify workers as independent contractors while exercising control over pay, deactivation, and work allocation. Lobby for third-category legislation that codifies flexibility while avoiding full employment costs (payroll taxes, benefits, unemployment insurance). Collect the surplus between platform fees and what full employment would cost.
narrative_ontology:constraint_stakeholder(employment_boundary__hybrid_security_reading, platform_companies, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(employment_boundary__hybrid_security_reading, platform_companies, beneficiary).

% Depend on platform work for primary income but lack career progression paths, skill development investments, promotion ladders, or tenure-based wage growth. The hybrid category provides injury insurance (86.2% coverage) and medical access (91.5%) but no retirement contributions, paid leave, or protection from algorithmic deactivation. Exit to traditional employment requires retraining and accepts wage cuts; exit to true independent contracting requires capital and client acquisition they cannot access.
narrative_ontology:constraint_stakeholder(employment_boundary__hybrid_security_reading, platform_workers_career_precarious, payer,
    organized, biographical, constrained, national).

% Older platform workers who have spent 5-15 years in the sector with no pension accrual, no employer retirement contributions, and no social security credits beyond self-employment minimums. Their professional identity is fused to platform work ('I am a driver,' 'I am a courier'), making exit psychologically and socially costly. The hybrid category's injury insurance covers acute harm but does nothing for the chronic extraction of retirement security.
narrative_ontology:constraint_stakeholder(employment_boundary__hybrid_security_reading, platform_workers_retirement_insecure, payer,
    moderate, generational, identity_locked, national).

% Workers (often students, caregivers, multi-job holders) who genuinely value schedule autonomy and multi-platform switching. For them, the hybrid category's basic protections (injury, medical) are a net gain over pure independent contracting, and the lack of career/retirement structure is an acceptable trade-off. They can exit to other gig platforms or traditional part-time work with relatively low friction.
narrative_ontology:constraint_stakeholder(employment_boundary__hybrid_security_reading, flexibility_preferring_workers, beneficiary,
    moderate, biographical, mobile, national).

% Workers in standard employment relationships whose bargaining power and regulatory floor are eroded when a growing workforce segment operates under a lower-protection regime. They would object to boundary dilution but are not consulted in platform-specific legislation. Their unions advocate for substantive employment classification but lack standing in platform-worker-specific hearings.
narrative_ontology:constraint_stakeholder(employment_boundary__hybrid_security_reading, traditional_employees, excluded,
    organized, generational, constrained, national).

% Administer the hybrid classification, certify platform compliance with injury/medical mandates, and adjudicate misclassification disputes. They see the full structure: the protections are real but partial, the extraction is measurable, and the category's stability depends on preventing both full employment reclassification and pure contractor deregulation.
narrative_ontology:constraint_stakeholder(employment_boundary__hybrid_security_reading, labor_regulators, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a legislative floor for platform workers who would otherwise have zero protections under independent contractor status: mandates injury insurance (86.2% coverage) and medical access (91.5%) without requiring platforms to bear full employment costs, creating a politically viable compromise.
% TRANSFER_FUNCTION: Moves the cost of basic injury/medical protection from workers (who would bear it entirely as contractors) and from the state (which would subsidize uninsured injuries) onto platforms, while platforms retain the surplus from avoiding payroll taxes, retirement contributions, unemployment insurance, and paid leave obligations.
% ABSENT_VOICES: Traditional employees and their unions are excluded from platform-specific legislative hearings; retired platform workers (who bear the retirement insecurity) are not organized; would-be platform workers deterred by the precarity have no voice. The flexibility_preferring_worker seat is real but over-represented in platform-funded surveys.
% DISAPPEARANCE_RATIONALE: If the hybrid category vanished overnight, platforms would face binary choice: reclassify as employees (massive cost increase, operational restructuring) or revert to pure independent contractor status (workers lose injury/medical floor, political backlash intensifies). Several jurisdictions have already triggered this rearrangement via court rulings (e.g., California AB5, EU Platform Work Directive), confirming the world rearranges.
% FOUNDING_PROBLEM: Platform work created a labor segment that fit neither the employment model (algorithmic control without human supervision, schedule autonomy) nor the contractor model (economic dependence, unilateral control by platform). Workers had zero protections; platforms faced misclassification litigation risk. The hybrid category was built to solve: (1) immediate worker harm from zero protection, (2) platform legal uncertainty, (3) political pressure for action without full employment mandates.
% FOUNDING_PROBLEM_CORROBORATION: Platform companies attest the problem is live (algorithmic management remains novel, flexibility demands persist). Labor economists (e.g., Katz & Krueger 2019 follow-ups, ILO 2021 platform work reports) and worker organizations attest the founding problem has shifted: the zero-protection baseline is solved in jurisdictions with hybrid laws, but the career/retirement gap has become the new live problem. No independent body attests the original founding problem remains the primary driver.
narrative_ontology:disappearance_verdict(employment_boundary__hybrid_security_reading, world_rearranges).
narrative_ontology:founding_problem_status(employment_boundary__hybrid_security_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(employment_boundary__hybrid_security_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(employment_boundary__hybrid_security_reading, 'none', 1).
narrative_ontology:epsilon_provenance(employment_boundary__hybrid_security_reading, 0.52, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(employment_boundary__hybrid_security_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(employment_boundary__hybrid_security_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(employment_boundary__hybrid_security_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.52: platforms avoid full employment costs (estimated 30-40% of payroll) while workers lose retirement security (generational cost) and career development. The injury/medical floor is real but covers only acute risks. Suppression 0.45: workers cannot access full employment protections without litigation; the hybrid category is legally entrenched and requires active legislative maintenance. Theater_ratio 0.38: the protections are genuine (not pure theater) but the category's framing as 'tailored protection' obscures the structural extraction. Accessibility_collapse 0.55: true independent contracting is inaccessible (no capital/client base); traditional employment requires exit costs (retraining, wage cut). Resistance 0.58: worker organizing (e.g., App Drivers Union, Gig Workers Rising), platform lobbying, and legislative battles in multiple jurisdictions show active contestation.
 *
 * PERSPECTIVAL GAP:
 *   From the platform seat, the hybrid category is a rope: it solves the coordination problem of 'how to provide basic protections without destroying the model.' From the career_precarious worker seat, it is a tangled_rope: real injury/medical coordination + extraction of career/retirement security. From the retirement_insecure worker seat, it approaches snare: the injury floor is real but the generational extraction (no pension, no tenure) is the dominant experience. The engine computes this per-seat divergence from the structural data; the authored claim (tangled_rope) reflects the constraint's aggregate structure.
 *
 * DIRECTIONALITY LOGIC:
 *   Platform companies are agenda_setters and beneficiaries: they write the rules via lobbying, collect the cost avoidance surplus, and have arbitrage-grade exit (can threaten market withdrawal). Platform_workers_career_precarious are payers with constrained exit: they need the income, cannot easily retrain, and the hybrid category is their only legal floor. Platform_workers_retirement_insecure are payers with identity_locked exit: their professional self-concept is fused to platform work, making exit psychologically costly even when economically rational. Flexibility_preferring_workers are beneficiaries with mobile exit: they gain protections without career expectations and can leave. Traditional_employees are excluded: they bear systemic erosion of the employment floor but have no seat in platform-specific lawmaking. Labor_regulators are observers: they administer the constraint and see its full structure.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (zero protections + legal uncertainty) is substantially solved in jurisdictions with hybrid laws. The constraint now persists because: (1) platforms extract surplus from the employment-cost gap, (2) flexibility_preferring_workers form a genuine beneficiary constituency, (3) regulators have built administrative infrastructure around the category, (4) neither full employment nor pure contracting has a winning coalition. This is mandatrophy: the mandate (protect workers from zero-protection baseline) has been met, but the constraint expands to cover the retirement/career gap it does not solve. The hybrid category institutionalizes precarity while claiming protection.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_structure,
    'Does the hybrid_security_reading represent a genuinely distinct structural category, or is it an unstable compromise that will collapse into either formalist or substantive classification?',
    'Longitudinal tracking of jurisdictions with hybrid laws: if hybrid categories persist >15 years without migrating to full employment or pure contracting, the third category is structurally stable. If they collapse, the reading was a transitional scaffold.',
    'If unstable scaffold, claimed_type should be scaffold (with sunset clause) not tangled_rope. If stable, tangled_rope stands. Affects whether the extraction is transitional or institutionalized.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_structure, empirical, 'Structural stability of the third labor category as a persistent regime vs. transitional compromise.').

omega_variable(
    retirement_extraction_measurement,
    'How much of the measured extractiveness (0.52) is attributable to the retirement/career gap vs. the injury/medical floor''s inadequacy?',
    'Counterfactual modeling: compute platform cost differential between hybrid category and full employment, decomposed into (a) injury/medical mandate costs, (b) retirement/payroll tax/benefits costs, (c) administrative/compliance costs. Compare to worker lifetime value loss from missing retirement contributions.',
    'If retirement gap dominates extraction, the constraint''s extraction is generational and cumulative (piton trajectory). If injury/medical floor inadequacy dominates, extraction is acute and potentially fixable by raising the floor without full employment.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(retirement_extraction_measurement, empirical, 'Decomposition of extraction sources: acute protection gaps vs. generational retirement/career extraction.').

omega_variable(
    flexibility_constituency_genuineness,
    'Is the flexibility_preferring_worker beneficiary seat a genuine preference constituency, or is it manufactured by platform survey methodology and algorithmic scheduling that makes flexibility appear chosen?',
    'Independent stated-preference studies with non-platform recruitment, controlling for income necessity and multi-platform availability. Compare revealed preference (actual switching behavior) to stated preference.',
    'If manufactured, the beneficiary seat shrinks, the constraint moves toward snare. If genuine, the coordination function is real and the tangled_rope classification holds with a real beneficiary constituency.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(flexibility_constituency_genuineness, empirical, 'Whether the flexibility beneficiary constituency is structurally real or platform-engineered.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(employment_boundary__hybrid_security_reading, 2015, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(empl_tr_t2015, employment_boundary__hybrid_security_reading, theater_ratio, 2015, 0.25).
narrative_ontology:measurement(empl_tr_t2017, employment_boundary__hybrid_security_reading, theater_ratio, 2017, 0.3).
narrative_ontology:measurement(empl_tr_t2019, employment_boundary__hybrid_security_reading, theater_ratio, 2019, 0.34).
narrative_ontology:measurement(empl_tr_t2021, employment_boundary__hybrid_security_reading, theater_ratio, 2021, 0.36).
narrative_ontology:measurement(empl_tr_t2023, employment_boundary__hybrid_security_reading, theater_ratio, 2023, 0.38).
narrative_ontology:measurement(empl_tr_t2025, employment_boundary__hybrid_security_reading, theater_ratio, 2025, 0.38).

% Extraction over time
narrative_ontology:measurement(empl_be_t2015, employment_boundary__hybrid_security_reading, base_extractiveness, 2015, 0.35).
narrative_ontology:measurement(empl_be_t2017, employment_boundary__hybrid_security_reading, base_extractiveness, 2017, 0.42).
narrative_ontology:measurement(empl_be_t2019, employment_boundary__hybrid_security_reading, base_extractiveness, 2019, 0.48).
narrative_ontology:measurement(empl_be_t2021, employment_boundary__hybrid_security_reading, base_extractiveness, 2021, 0.51).
narrative_ontology:measurement(empl_be_t2023, employment_boundary__hybrid_security_reading, base_extractiveness, 2023, 0.52).
narrative_ontology:measurement(empl_be_t2025, employment_boundary__hybrid_security_reading, base_extractiveness, 2025, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(empl_su_t2015, employment_boundary__hybrid_security_reading, suppression_requirement, 2015, 0.3).
narrative_ontology:measurement(empl_su_t2017, employment_boundary__hybrid_security_reading, suppression_requirement, 2017, 0.38).
narrative_ontology:measurement(empl_su_t2019, employment_boundary__hybrid_security_reading, suppression_requirement, 2019, 0.42).
narrative_ontology:measurement(empl_su_t2021, employment_boundary__hybrid_security_reading, suppression_requirement, 2021, 0.44).
narrative_ontology:measurement(empl_su_t2023, employment_boundary__hybrid_security_reading, suppression_requirement, 2023, 0.45).
narrative_ontology:measurement(empl_su_t2025, employment_boundary__hybrid_security_reading, suppression_requirement, 2025, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(employment_boundary__hybrid_security_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(employment_boundary__hybrid_security_reading, 0.18).
narrative_ontology:affects_constraint(employment_boundary__hybrid_security_reading, employment_boundary__formalist_employment_reading).
narrative_ontology:affects_constraint(employment_boundary__hybrid_security_reading, employment_boundary__substantive_employment_reading).
narrative_ontology:affects_constraint(employment_boundary__hybrid_security_reading, platform_algorithmic_control).
narrative_ontology:affects_constraint(employment_boundary__hybrid_security_reading, social_protection_floor).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the employment_boundary kernel. The formalist reading treats the boundary as a contract-law fact (low extraction, mountain-like). The substantive reading treats it as economic-dependence fact (high extraction, snare-like). This hybrid reading treats it as a regulatory compromise (moderate extraction, tangled_rope). The three readings form a constraint family linked by mutual network.affects_constraints references.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(employment_boundary__hybrid_security_reading, organized, 0.65).
constraint_indexing:directionality_override(employment_boundary__hybrid_security_reading, moderate, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
