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
 *   human_readable: Hybrid Security Reading: Platform Worker Third-Category Protections
 *   domain: labor_economics/platform_economy/social_policy
 *
 * SUMMARY:
 *   The hybrid_security_reading instantiates a third-category constraint for
 *   platform workers: neither employee nor independent contractor, but a
 *   tailored protection regime (medical 91.5%, injury 86.2% coverage) with
 *   platform obligations limited to injury insurance. This reading claims to
 *   solve the coordination problem of extending basic protections without
 *   imposing full employment costs. The metrics describe a constraint that
 *   has grown more extractive and theatrical over 12 years — extraction
 *   rising from 0.28 to 0.42 as platforms institutionalize the hybrid
 *   category while resisting expansion of obligations; theater rising from
 *   0.15 to 0.31 as 'flexibility' framing increasingly covers precarity;
 *   suppression hardening from 0.22 to 0.38 as algorithmic management
 *   replaces contractual control.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(employment_boundary__hybrid_security_reading, 0.42).
domain_priors:suppression_score(employment_boundary__hybrid_security_reading, 0.38).
domain_priors:theater_ratio(employment_boundary__hybrid_security_reading, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(employment_boundary__hybrid_security_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(employment_boundary__hybrid_security_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(employment_boundary__hybrid_security_reading, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(employment_boundary__hybrid_security_reading, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(employment_boundary__hybrid_security_reading, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(employment_boundary__hybrid_security_reading, tangled_rope).
narrative_ontology:human_readable(employment_boundary__hybrid_security_reading, "Hybrid Security Reading: Platform Worker Third-Category Protections").
narrative_ontology:topic_domain(employment_boundary__hybrid_security_reading, "labor_economics/platform_economy/social_policy").

domain_priors:requires_active_enforcement(employment_boundary__hybrid_security_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(employment_boundary__hybrid_security_reading, 'bc4f65ed-6f71-429b-a24b-49b9cef2e720').
narrative_ontology:cs_kernel_codification('bc4f65ed-6f71-429b-a24b-49b9cef2e720', distributed).
narrative_ontology:cs_authority_grounding('bc4f65ed-6f71-429b-a24b-49b9cef2e720', distributed).
narrative_ontology:cs_reading_relation('bc4f65ed-6f71-429b-a24b-49b9cef2e720', employment_boundary__formalist_employment_reading, coexists_with).
narrative_ontology:cs_reading_relation('bc4f65ed-6f71-429b-a24b-49b9cef2e720', employment_boundary__substantive_employment_reading, influences).
narrative_ontology:cs_axiom('bc4f65ed-6f71-429b-a24b-49b9cef2e720', foundational, third_category_necessity).
narrative_ontology:cs_axiom_status(third_category_necessity, holdable).
narrative_ontology:cs_axiom_grounding('bc4f65ed-6f71-429b-a24b-49b9cef2e720', third_category_necessity, instrumental).
narrative_ontology:cs_axiom('bc4f65ed-6f71-429b-a24b-49b9cef2e720', foundational, protection_without_employment_costs).
narrative_ontology:cs_axiom_status(protection_without_employment_costs, holdable).
narrative_ontology:cs_axiom_grounding('bc4f65ed-6f71-429b-a24b-49b9cef2e720', protection_without_employment_costs, conventional).
narrative_ontology:cs_reference_frame('bc4f65ed-6f71-429b-a24b-49b9cef2e720', pre_platform_zero_protections_baseline).
narrative_ontology:cs_drift_state('bc4f65ed-6f71-429b-a24b-49b9cef2e720', post_algorithmic_control_evidence, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('bc4f65ed-6f71-429b-a24b-49b9cef2e720', '').
narrative_ontology:cs_kernel_id(employment_boundary__hybrid_security_reading, employment_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(employment_boundary__hybrid_security_reading, platform_companies).
narrative_ontology:constraint_beneficiary(employment_boundary__hybrid_security_reading, platform_workers_basic_protections).
narrative_ontology:constraint_victim(employment_boundary__hybrid_security_reading, platform_workers_long_term_precariat).
narrative_ontology:constraint_vindicates(employment_boundary__hybrid_security_reading, tailored_third_category_necessity).
narrative_ontology:constraint_vindicates(employment_boundary__hybrid_security_reading, flexibility_security_tradeoff_justified).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design the hybrid category through lobbying, litigation, and platform architecture. Collect the value of labor without long-term reproduction costs (pensions, career development, unemployment insurance). Obligated only for injury insurance (86.2% coverage) and basic medical (91.5%). Can shift jurisdiction, reclassify work, or alter algorithmic parameters to maintain the hybrid ceiling.
narrative_ontology:constraint_stakeholder(employment_boundary__hybrid_security_reading, platform_companies, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(employment_boundary__hybrid_security_reading, platform_companies, beneficiary).

% Receive immediate medical and injury protections that did not exist before the hybrid category. Coverage rates are high (91.5% medical, 86.2% injury) but benefits are event-driven, not accumulative. Exit options are constrained: leaving platform work means losing even these basic protections; staying means accepting no career progression, no retirement vesting, no seniority. The 'flexibility' narrative frames this as choice.
narrative_ontology:constraint_stakeholder(employment_boundary__hybrid_security_reading, platform_workers_basic_protections, beneficiary,
    organized, biographical, constrained, regional).

% Same workers as above, but viewed over the biographical horizon. They bear the extraction: no pension contributions, no skill certification portability, no unemployment insurance, no career ladder. Algorithmic management internalizes precarity — deactivation risk suppresses wage demands and organizing. Identity-locked because platform work becomes primary identity and income source; exit means not just job loss but identity rupture. The hybrid category institutionalizes this as 'independent entrepreneurship.'
narrative_ontology:constraint_stakeholder(employment_boundary__hybrid_security_reading, platform_workers_long_term_precariat, payer,
    moderate, biographical, identity_locked, national).

% Compete with platforms that externalize labor reproduction costs. Would face pressure to adopt hybrid categories if the model spreads. Not in the conversation because the hybrid category is framed as 'new economy' specific. Their objection — competitive distortion — is structurally excluded from the policy framing.
narrative_ontology:constraint_stakeholder(employment_boundary__hybrid_security_reading, traditional_employers, excluded,
    institutional, generational, mobile, national).

% Administer the hybrid category, monitor coverage rates, adjudicate boundary disputes. They see the full structure: the coordination function (basic protections delivered) and the extraction function (long-term obligations avoided). Their enforcement capacity is limited by the 'innovation' framing that treats algorithmic management as outside labor law.
narrative_ontology:constraint_stakeholder(employment_boundary__hybrid_security_reading, labor_regulators, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Extends basic medical and injury protections to workers who would otherwise have none, without imposing full employment costs that platforms claim would destroy the business model. Solves the 'zero protections' coordination failure for a workforce that falls through the employee/contractor binary.
% TRANSFER_FUNCTION: Moves long-term labor reproduction costs (pensions, unemployment insurance, career infrastructure, skill certification) from platforms to individual workers and the public purse, while platforms retain the value of algorithmically coordinated labor. Basic medical/injury costs stay with platforms (91.5%/86.2% coverage) — the transfer is selective.
% ABSENT_VOICES: Traditional employers facing competitive distortion from platforms that externalize reproduction costs. Workers who would be employees under substantive_employment_reading but are locked into hybrid category. Public pension/unemployment systems that absorb the long-term costs. These voices are excluded by the 'new economy exceptionalism' framing.
% DISAPPEARANCE_RATIONALE: If the hybrid category vanished overnight, platforms would face binary choice: classify workers as employees (full obligations) or contractors (zero protections). Most jurisdictions would likely push toward substantive employment classification given algorithmic control evidence. The platform labor market would reorganize around either full employment costs or genuinely independent contracting with portable benefits.
% FOUNDING_PROBLEM: Platform workers had zero protections — no medical coverage, no injury insurance, no safety net — because they fell outside both employment and contractor categories. The hybrid category was built to deliver basic protections without destroying platform viability.
% FOUNDING_PROBLEM_CORROBORATION: Platform companies and some labor ministries attest the problem remains live (new platform sectors emerging, coverage gaps persist). Worker organizations and independent researchers attest the founding problem is substantially solved for acute risks (medical/injury) but the arrangement now prevents solving the chronic risks (retirement, career, collective voice) — the hybrid category has become a ceiling. No neutral arbiter has declared the problem dead.
narrative_ontology:disappearance_verdict(employment_boundary__hybrid_security_reading, world_rearranges).
narrative_ontology:founding_problem_status(employment_boundary__hybrid_security_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(employment_boundary__hybrid_security_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(employment_boundary__hybrid_security_reading, 'none', 1).
narrative_ontology:epsilon_provenance(employment_boundary__hybrid_security_reading, 0.42, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(employment_boundary__hybrid_security_reading_tests).
:- end_tests(employment_boundary__hybrid_security_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.42) reflects the gap between full employment cost and hybrid obligation cost — platforms avoid payroll taxes, unemployment insurance, pension contributions, and career infrastructure while capturing labor. Suppression (0.38) is algorithmic: deactivation without appeal, rating systems that penalize advocacy, contractual waivers of collective action. Theater (0.31) is the 'innovation' narrative that presents precarity as worker choice. The constraint is tangled_rope because it delivers real coordination (basic medical/injury coverage where none existed) AND asymmetric extraction (platforms shed long-term obligations). Active enforcement is required: platforms lobby to maintain the third category, litigate against reclassification, and design algorithmic controls that prevent worker organizing.
 *
 * PERSPECTIVAL GAP:
 *   From the platform seat, the hybrid category is a rope: genuine coordination solving the 'no protections at all' problem with minimal coercion. From the long-term platform worker seat, it is a snare: the coordination story covers extraction of career development, retirement security, and collective voice. The engine computes this divergence from the structural data — the same constraint, different effective extraction per seat.
 *
 * DIRECTIONALITY LOGIC:
 *   Platform companies are structural beneficiaries (d ~0.15): they collect the labor value while externalizing long-term reproduction costs. Platform workers are split: those receiving basic protections are partial beneficiaries (d ~0.45) but those lacking career/retirement infrastructure are victims (d ~0.75) — the same workers occupy both positions over their lifecycle. Regulators are observers (d ~0.5). The split within platform_workers is the key seat divergence: the constraint coordinates immediate injury/medical needs while extracting long-term security.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (no protections for gig workers) is partially solved — medical/injury coverage exists. But the arrangement now prevents the substantive_employment_reading from gaining traction by offering a 'good enough' alternative that stabilizes platform business models. Mandatrophy is NOT resolved: the hybrid category has become a ceiling, not a floor.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint a genuine alternative to the employment boundary or a mechanism that stabilizes the boundary by offering partial concessions?',
    'Track whether hybrid protections expand toward full employment equivalence over time or remain a stable ceiling. Longitudinal policy adoption data across jurisdictions.',
    'If the hybrid category is a stable ceiling, it functions as a pressure valve preventing substantive employment classification — extraction is structural. If it ratchets upward, it is a transitional scaffold toward substantive employment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether hybrid_security_reading forecloses or coexists with substantive_employment_reading in practice').

omega_variable(
    extraction_distribution_ambiguity,
    'Does the 0.42 extractiveness primarily reflect platform cost savings from avoided employment obligations, or worker losses from absent career/retirement infrastructure?',
    'Decompose the extraction into platform-side avoided costs (employer payroll taxes, benefits administration, unemployment insurance) vs worker-side forgone accumulations (pension vesting, seniority, skill certification portability).',
    'If extraction is predominantly platform cost avoidance, the beneficiary is the platform. If predominantly worker foregone accumulation, the victim bears a structural loss not captured by current benefit rates.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(extraction_distribution_ambiguity, empirical, 'Attribution of hybrid category extraction between platform savings and worker precarity').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression (0.38) structural (algorithmic deactivation, contractual prohibition on collective bargaining) or internalized (workers accept precarity as ''flexibility'' trade-off)?',
    'Post-exit suppression trajectory: survey workers who leave platform work — if precarity acceptance persists, reclassify as partially internalized. Compare deactivation appeal success rates vs traditional unfair dismissal.',
    'If internalized, effective suppression is higher than structural measure — the constraint travels with the worker. If structural, suppression drops sharply at exit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized suppression in algorithmic management').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(employment_boundary__hybrid_security_reading, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hybrid_sec_tr_t0, employment_boundary__hybrid_security_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(hybrid_sec_tr_t3, employment_boundary__hybrid_security_reading, theater_ratio, 3, 0.19).
narrative_ontology:measurement(hybrid_sec_tr_t6, employment_boundary__hybrid_security_reading, theater_ratio, 6, 0.24).
narrative_ontology:measurement(hybrid_sec_tr_t9, employment_boundary__hybrid_security_reading, theater_ratio, 9, 0.28).
narrative_ontology:measurement(hybrid_sec_tr_t12, employment_boundary__hybrid_security_reading, theater_ratio, 12, 0.31).

% Extraction over time
narrative_ontology:measurement(hybrid_sec_be_t0, employment_boundary__hybrid_security_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(hybrid_sec_be_t3, employment_boundary__hybrid_security_reading, base_extractiveness, 3, 0.32).
narrative_ontology:measurement(hybrid_sec_be_t6, employment_boundary__hybrid_security_reading, base_extractiveness, 6, 0.36).
narrative_ontology:measurement(hybrid_sec_be_t9, employment_boundary__hybrid_security_reading, base_extractiveness, 9, 0.39).
narrative_ontology:measurement(hybrid_sec_be_t12, employment_boundary__hybrid_security_reading, base_extractiveness, 12, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(hybrid_sec_su_t0, employment_boundary__hybrid_security_reading, suppression_requirement, 0, 0.22).
narrative_ontology:measurement(hybrid_sec_su_t3, employment_boundary__hybrid_security_reading, suppression_requirement, 3, 0.27).
narrative_ontology:measurement(hybrid_sec_su_t6, employment_boundary__hybrid_security_reading, suppression_requirement, 6, 0.31).
narrative_ontology:measurement(hybrid_sec_su_t9, employment_boundary__hybrid_security_reading, suppression_requirement, 9, 0.35).
narrative_ontology:measurement(hybrid_sec_su_t12, employment_boundary__hybrid_security_reading, suppression_requirement, 12, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(employment_boundary__hybrid_security_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(employment_boundary__hybrid_security_reading, 0.12).
narrative_ontology:affects_constraint(employment_boundary__hybrid_security_reading, employment_boundary__formalist_employment_reading).
narrative_ontology:affects_constraint(employment_boundary__hybrid_security_reading, employment_boundary__substantive_employment_reading).
narrative_ontology:affects_constraint(employment_boundary__hybrid_security_reading, platform_algorithmic_control).
narrative_ontology:affects_constraint(employment_boundary__hybrid_security_reading, portable_benefits_infrastructure).

% DUAL FORMULATION NOTE:
% The employment_boundary kernel decomposes into three constraint stories with distinct ε values: formalist (low ε, mountain-like), hybrid_security (moderate ε = 0.42, tangled_rope), substantive (high ε if platforms resist, snare/tangled_rope). The hybrid reading sits between them structurally — it concedes basic protections (moving toward substantive) but blocks full employment classification (preserving formalist boundary). Network edges reflect this: hybrid influences both siblings by occupying the contested middle ground.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(employment_boundary__hybrid_security_reading, organized, 0.75).
constraint_indexing:directionality_override(employment_boundary__hybrid_security_reading, moderate, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
