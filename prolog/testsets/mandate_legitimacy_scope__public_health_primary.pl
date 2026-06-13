% ============================================================================
% CONSTRAINT STORY: mandate_legitimacy_scope__public_health_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_mandate_legitimacy_scope__public_health_primary, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: mandate_legitimacy_scope__public_health_primary
 *   human_readable: Public Health Mandate Legitimacy (Collective Protection Reading)
 *   domain: public_health/constitutional_law/medical_autonomy
 *
 * SUMMARY:
 *   This constraint embodies one reading of the contested kernel
 *   'mandate_legitimacy_scope': the claim that state authority to compel
 *   vaccination derives its legitimacy from the duty to protect vulnerable
 *   populations who cannot protect themselves. Under this reading, the
 *   unvaccinated-by-choice bear a responsibility to protect the
 *   immunocompromised and the unvaccinated-unable. The founding
 *   problem—outbreaks harming vulnerable populations when coverage
 *   lapses—provides the justification. This reading COEXISTS WITH but
 *   competes against the bodily_autonomy_primary reading (medical
 *   intervention without consent violates fundamental rights) and the
 *   proportionality_reading (legitimacy depends on disease severity, vaccine
 *   safety, and availability of less restrictive alternatives). The
 *   constraint operates by transferring medical decision-making authority
 *   from individuals to public health institutions, justified by protection
 *   duties to the most vulnerable.
 *
 * KEY AGENTS:
 *   - public_health_authority: sets mandate scope and enforces via institutional exclusion (schools, employment, facilities)
 *   - immunocompromised_populations: powerless beneficiaries; their protection is the mandate's structural justification
 *   - vaccine_hesitant_adults: moderate-power payers; face employment/education exclusion and social isolation if noncompliant
 *   - individuals_with_contraindications: moderate-power payers with identity lock; cannot vaccinate but must navigate exemption gatekeeping
 *   - employers_and_institutions: institutional agenda-setters (derivative authority); implement mandate enforcement
 *   - excluded_alternative_voices: advocacy groups, autonomy-focused philosophers, alternative practitioners systematically excluded from policy authority
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(mandate_legitimacy_scope__public_health_primary, 0.62).
domain_priors:suppression_score(mandate_legitimacy_scope__public_health_primary, 0.71).
domain_priors:theater_ratio(mandate_legitimacy_scope__public_health_primary, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(mandate_legitimacy_scope__public_health_primary, extractiveness, 0.62).
narrative_ontology:constraint_metric(mandate_legitimacy_scope__public_health_primary, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(mandate_legitimacy_scope__public_health_primary, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(mandate_legitimacy_scope__public_health_primary, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(mandate_legitimacy_scope__public_health_primary, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(mandate_legitimacy_scope__public_health_primary, tangled_rope).
narrative_ontology:human_readable(mandate_legitimacy_scope__public_health_primary, "Public Health Mandate Legitimacy (Collective Protection Reading)").
narrative_ontology:topic_domain(mandate_legitimacy_scope__public_health_primary, "public_health/constitutional_law/medical_autonomy").

domain_priors:requires_active_enforcement(mandate_legitimacy_scope__public_health_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(mandate_legitimacy_scope__public_health_primary, '51daf21b-3b2d-4c60-9cfa-c7be1a79a5e4').
narrative_ontology:cs_kernel_codification('51daf21b-3b2d-4c60-9cfa-c7be1a79a5e4', formalized).
narrative_ontology:cs_authority_grounding('51daf21b-3b2d-4c60-9cfa-c7be1a79a5e4', extraction).
narrative_ontology:cs_interpretation_layer_present('51daf21b-3b2d-4c60-9cfa-c7be1a79a5e4').
narrative_ontology:cs_reading_relation('51daf21b-3b2d-4c60-9cfa-c7be1a79a5e4', mandate_legitimacy_scope__bodily_autonomy_primary, coexists_with).
narrative_ontology:cs_reading_relation('51daf21b-3b2d-4c60-9cfa-c7be1a79a5e4', mandate_legitimacy_scope__proportionality_reading, influences).
narrative_ontology:cs_axiom('51daf21b-3b2d-4c60-9cfa-c7be1a79a5e4', foundational, duty_to_protect_vulnerable_overrides_autonomy).
narrative_ontology:cs_axiom_status(duty_to_protect_vulnerable_overrides_autonomy, holdable).
narrative_ontology:cs_axiom_grounding('51daf21b-3b2d-4c60-9cfa-c7be1a79a5e4', duty_to_protect_vulnerable_overrides_autonomy, deontological).
narrative_ontology:cs_axiom('51daf21b-3b2d-4c60-9cfa-c7be1a79a5e4', foundational, state_medical_authority_legitimate_for_collective_protection).
narrative_ontology:cs_axiom_status(state_medical_authority_legitimate_for_collective_protection, holdable).
narrative_ontology:cs_axiom_grounding('51daf21b-3b2d-4c60-9cfa-c7be1a79a5e4', state_medical_authority_legitimate_for_collective_protection, deontological).
narrative_ontology:cs_reference_frame('51daf21b-3b2d-4c60-9cfa-c7be1a79a5e4', vulnerable_population_protection_framework).
narrative_ontology:cs_drift_state('51daf21b-3b2d-4c60-9cfa-c7be1a79a5e4', post_endemic_disease_transition, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('51daf21b-3b2d-4c60-9cfa-c7be1a79a5e4', '').
narrative_ontology:cs_kernel_id(mandate_legitimacy_scope__public_health_primary, mandate_legitimacy_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(mandate_legitimacy_scope__public_health_primary, immunocompromised_populations).
narrative_ontology:constraint_beneficiary(mandate_legitimacy_scope__public_health_primary, infants_and_elderly).
narrative_ontology:constraint_victim(mandate_legitimacy_scope__public_health_primary, vaccine_hesitant_adults).
narrative_ontology:constraint_victim(mandate_legitimacy_scope__public_health_primary, individuals_with_contraindications).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(mandate_legitimacy_scope__public_health_primary, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(mandate_legitimacy_scope__public_health_primary, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(mandate_legitimacy_scope__public_health_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(mandate_legitimacy_scope__public_health_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(mandate_legitimacy_scope__public_health_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-to-high (0.62 at interval end) because the constraint transfers bodily decision-making to the state with no direct reciprocal agency for the payers (vaccine-hesitant individuals cannot vote the mandate out; individuals with contraindications face high proof burdens for exemptions). Suppression is high (0.71) because noncompliance carries severe institutional penalties (employment loss, educational exclusion, social restriction) with limited appeal mechanisms. Theater is moderate-low (0.28) initially, rising to moderate (0.28 plateau) as the constraint matures: the genuine disease-protection function remains real, but as vaccination becomes routine and outbreaks disappear, more enforcement activity defends mandate legitimacy itself rather than addressing immediate disease threat. The measurement series track extractiveness rising then plateauing (foundational mandate established, scope stable), suppression rising then plateauing (enforcement machinery scaled and normalized), and theater stable-to-rising (the constraint's public justification becomes less tied to current disease risk and more tied to institutional continuity and past harm). All metrics are authored on one shared time grid.
 *
 * PERSPECTIVAL GAP:
 *   The payer and beneficiary seats experience this constraint entirely differently. Immunocompromised populations and infants perceive it as protective coordination—a solution to their existential vulnerability. Vaccine-hesitant adults perceive it as coercive extraction of bodily autonomy with no seat in governance. Individuals with contraindications perceive it as a system that claims to protect them but gates their exemptions behind authority-defined medical criteria they don't control. The public health authority perceives it as legitimate collective-action problem solving grounded in duties to vulnerable populations. The engine should compute dramatically different types across these seats—protection vs. extraction vs. identity-lock—from the structural data alone.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries: immunocompromised (d → 0.0), infants/elderly (d → 0.0). Payers: vaccine-hesitant (d → 0.75, constrained exit, penalty-heavy), individuals with contraindications (d → 0.8, identity-locked, exemption gatekeeping). Agenda-setter: public_health_authority (d → 0.15, institutional power, no personal penalty). The constraint's extraction is amplified for trapped/identity-locked payers and damped for beneficiaries with zero alternatives—the very asymmetry this reading claims is justified.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading avoids mislabeling the constraint as pure rope (which would imply symmetric participant benefit and genuine voluntary entry). The tangled_rope classification captures the core structure: real coordination function (herd immunity protection) coupled with asymmetric extraction (bodily decision authority transferred with no reciprocal agency for payers). The classification prevents the constraint from passing as benign coordination when it is sustained by suppression (institutional penalties) and benefits flow structurally to those least able to object. The constraint requires active enforcement (institutional exclusion, exemption gatekeeping, employment verification) to persist; voluntary uptake alone is not sufficient. This is not a rope; it is a tangled_rope where coordination and extraction are structurally fused.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coercion_vs_protection_boundary,
    'At what vaccination coverage rate does the constraint shift from solving a genuine collective-action problem (covering the unvaccinated-unable) to enforcing compliance beyond what herd immunity requires?',
    'Epidemiological analysis establishing minimum herd-immunity thresholds for each disease, compared against actual mandate enforcement coverage levels over time. Natural experiments from jurisdictions with different coverage targets.',
    'If enforcement exceeds epidemiological thresholds, the constraint contains a pure-extraction component beyond the stated protection function. If enforcement aligns with thresholds, the extraction is the unavoidable cost of the coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coercion_vs_protection_boundary, empirical, 'Whether the mandate extracts beyond what protecting vulnerable populations requires.').

omega_variable(
    contraindication_gatekeeping_symmetry,
    'Are exemptions for genuine medical contraindications granted symmetrically to medical judgment, or does the same authority that sets the mandate also determine contraindication standards in ways that minimize exemptions?',
    'Comparative analysis of exemption approval rates across different medical conditions and jurisdictions; audit of appeal outcomes for denied exemptions; comparison of clinical contraindication prevalence against exemption rates.',
    'If gatekeeping is authority-asymmetric (authority both mandates and sets narrow exemption criteria), individuals with contraindications face identity-lock extraction (cannot opt out without medical disqualification). If gatekeeping is clinically symmetric, some exit option opens.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(contraindication_gatekeeping_symmetry, empirical, 'Whether exemption gatekeeping is structurally fair or a mechanism of extraction.').

omega_variable(
    vulnerable_population_agency,
    'Do immunocompromised and other vulnerable populations participate in setting mandate scope and enforcement terms, or are they structurally excluded from governance while remaining the stated beneficiaries?',
    'Audit of policy-setting bodies: what seats do vulnerable populations hold? How are their preferences elicited and weighted? Analysis of prior mandates where vulnerable population voice shaped scope.',
    'If vulnerable populations are excluded from governance while being the justification, their benefit is structurally asymmetric (they receive protection they did not choose). If included, they become co-agenda-setters and the structure is more genuinely coordinated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(vulnerable_population_agency, conceptual, 'Whether the constraint''s beneficiaries participate in its governance or remain subjects of authority.').

omega_variable(
    reading_coexistence_stability,
    'Can this reading (public_health_primary) and the bodily_autonomy_primary reading coexist indefinitely in the same legal/institutional framework, or does one eventually foreclose the other as state capacity and disease burden change?',
    'Historical analysis of constitutional interpretations and precedent: has the autonomy reading completely been abandoned, or does it resurface when disease threat recedes? Does legislative reauthorization of mandates imply ongoing contest?',
    'If coexistence is genuine and stable, the readings are siblings in a lived pluralism. If one is gradually foreclosing the other, the framework is resolving the contest, not hosting it. This affects how mandate legitimacy is interpreted in low-threat scenarios.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_coexistence_stability, conceptual, 'Whether the public_health and autonomy readings are genuinely coexisting or one is foreclosing the other.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(mandate_legitimacy_scope__public_health_primary, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mand_tr_t0, mandate_legitimacy_scope__public_health_primary, theater_ratio, 0, 0.18).
narrative_ontology:measurement(mand_tr_t5, mandate_legitimacy_scope__public_health_primary, theater_ratio, 5, 0.22).
narrative_ontology:measurement(mand_tr_t10, mandate_legitimacy_scope__public_health_primary, theater_ratio, 10, 0.26).
narrative_ontology:measurement(mand_tr_t15, mandate_legitimacy_scope__public_health_primary, theater_ratio, 15, 0.28).
narrative_ontology:measurement(mand_tr_t20, mandate_legitimacy_scope__public_health_primary, theater_ratio, 20, 0.28).
narrative_ontology:measurement(mand_tr_t25, mandate_legitimacy_scope__public_health_primary, theater_ratio, 25, 0.28).

% Extraction over time
narrative_ontology:measurement(mand_be_t0, mandate_legitimacy_scope__public_health_primary, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(mand_be_t5, mandate_legitimacy_scope__public_health_primary, base_extractiveness, 5, 0.54).
narrative_ontology:measurement(mand_be_t10, mandate_legitimacy_scope__public_health_primary, base_extractiveness, 10, 0.59).
narrative_ontology:measurement(mand_be_t15, mandate_legitimacy_scope__public_health_primary, base_extractiveness, 15, 0.61).
narrative_ontology:measurement(mand_be_t20, mandate_legitimacy_scope__public_health_primary, base_extractiveness, 20, 0.62).
narrative_ontology:measurement(mand_be_t25, mandate_legitimacy_scope__public_health_primary, base_extractiveness, 25, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(mand_su_t0, mandate_legitimacy_scope__public_health_primary, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(mand_su_t5, mandate_legitimacy_scope__public_health_primary, suppression_requirement, 5, 0.64).
narrative_ontology:measurement(mand_su_t10, mandate_legitimacy_scope__public_health_primary, suppression_requirement, 10, 0.69).
narrative_ontology:measurement(mand_su_t15, mandate_legitimacy_scope__public_health_primary, suppression_requirement, 15, 0.71).
narrative_ontology:measurement(mand_su_t20, mandate_legitimacy_scope__public_health_primary, suppression_requirement, 20, 0.71).
narrative_ontology:measurement(mand_su_t25, mandate_legitimacy_scope__public_health_primary, suppression_requirement, 25, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(mandate_legitimacy_scope__public_health_primary, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(mandate_legitimacy_scope__public_health_primary, 0.12).
narrative_ontology:affects_constraint(mandate_legitimacy_scope__public_health_primary, mandate_legitimacy_scope__bodily_autonomy_primary).
narrative_ontology:affects_constraint(mandate_legitimacy_scope__public_health_primary, mandate_legitimacy_scope__proportionality_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel 'mandate_legitimacy_scope'. The sibling readings—bodily_autonomy_primary and proportionality_reading—are separate constraint stories with different ε values, beneficiary/victim structures, and structural justifications. This reading (public_health_primary) achieves its coordination function by transferring medical decision authority to public health institutions; the bodily_autonomy_primary reading contests that transfer as a violation of fundamental rights; the proportionality_reading narrows the scope to only diseases meeting severity and safety thresholds. All three readings coexist as live political/legal positions held by different parties and jurisdictions. The network edges establish that this constraint's persistence is intertwined with the contested status of the kernel itself.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(mandate_legitimacy_scope__public_health_primary, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
