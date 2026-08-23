% ============================================================================
% CONSTRAINT STORY: coercion_legitimacy_boundary__bodily_autonomy_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-01-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_coercion_legitimacy_boundary__bodily_autonomy_primary, []).

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
 *   constraint_id: coercion_legitimacy_boundary__bodily_autonomy_primary
 *   human_readable: Categorical Bodily Autonomy Barrier
 *   domain: public_health_policy/medical_ethics/constitutional_law
 *
 * SUMMARY:
 *   This constraint instantiates the bodily_autonomy_primary reading of the
 *   coercion_legitimacy_boundary kernel. It holds that medical intervention
 *   without consent is categorically impermissible regardless of collective
 *   benefit, enforced through constitutional and medical-legal institutions.
 *   The constraint coordinates society around an absolute bodily integrity
 *   boundary but asymmetrically extracts health security from
 *   immunocompromised individuals who bear the externalized risk of others'
 *   refusals. It is claimed as tangled_rope: genuine coordination against
 *   state medical coercion layered with asymmetric harm to vulnerable
 *   populations.
 *
 * KEY AGENTS:
 *   - Medical refusers (moderate/mobile): Primary beneficiaries â shielded from coercion by the categorical boundary.
 *   - Immunocompromised individuals (powerless/trapped): Primary targets â bear concentrated infection risk from non-enforcement of preventive mandates.
 *   - Public health authorities (institutional/mobile): Beneficiaries per structural delta â relieved of mandate-enforcement burden and political accountability for coercion.
 *   - Healthcare providers (organized/constrained): Dual-positioned beneficiaries â protected from liability and also frontline enforcers of consent protocols.
 *   - Judiciary (institutional/analytical): Agenda setter â interprets and enforces the categorical boundary.
 *   - Public health advocates (organized/constrained): Excluded voices â argue for proportionate exceptions but are structurally sidelined by the categorical framing.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(coercion_legitimacy_boundary__bodily_autonomy_primary, 0.48).
domain_priors:suppression_score(coercion_legitimacy_boundary__bodily_autonomy_primary, 0.62).
domain_priors:theater_ratio(coercion_legitimacy_boundary__bodily_autonomy_primary, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__bodily_autonomy_primary, extractiveness, 0.48).
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__bodily_autonomy_primary, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__bodily_autonomy_primary, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__bodily_autonomy_primary, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__bodily_autonomy_primary, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(coercion_legitimacy_boundary__bodily_autonomy_primary, tangled_rope).
narrative_ontology:human_readable(coercion_legitimacy_boundary__bodily_autonomy_primary, "Categorical Bodily Autonomy Barrier").
narrative_ontology:topic_domain(coercion_legitimacy_boundary__bodily_autonomy_primary, "public_health_policy/medical_ethics/constitutional_law").

domain_priors:requires_active_enforcement(coercion_legitimacy_boundary__bodily_autonomy_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(coercion_legitimacy_boundary__bodily_autonomy_primary, 'b02c6581-3b68-4b80-82c0-9ad3ac3df889').
narrative_ontology:cs_kernel_codification('b02c6581-3b68-4b80-82c0-9ad3ac3df889', formalized).
narrative_ontology:cs_authority_grounding('b02c6581-3b68-4b80-82c0-9ad3ac3df889', lineage).
narrative_ontology:cs_interpretation_layer_present('b02c6581-3b68-4b80-82c0-9ad3ac3df889').
narrative_ontology:cs_reading_relation('b02c6581-3b68-4b80-82c0-9ad3ac3df889', coercion_legitimacy_boundary__public_health_primary, forecloses).
narrative_ontology:cs_reading_relation('b02c6581-3b68-4b80-82c0-9ad3ac3df889', coercion_legitimacy_boundary__proportionality_reading, forecloses).
narrative_ontology:cs_axiom('b02c6581-3b68-4b80-82c0-9ad3ac3df889', foundational, bodily_integrity_absolute).
narrative_ontology:cs_axiom_status(bodily_integrity_absolute, holdable).
narrative_ontology:cs_axiom_grounding('b02c6581-3b68-4b80-82c0-9ad3ac3df889', bodily_integrity_absolute, deontological).
narrative_ontology:cs_reference_frame('b02c6581-3b68-4b80-82c0-9ad3ac3df889', absolute_bodily_integrity).
narrative_ontology:cs_drift_state('b02c6581-3b68-4b80-82c0-9ad3ac3df889', contemporary_public_health_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('b02c6581-3b68-4b80-82c0-9ad3ac3df889', '').
narrative_ontology:cs_kernel_id(coercion_legitimacy_boundary__bodily_autonomy_primary, coercion_legitimacy_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(coercion_legitimacy_boundary__bodily_autonomy_primary, medical_refusers).
narrative_ontology:constraint_beneficiary(coercion_legitimacy_boundary__bodily_autonomy_primary, public_health_authorities).
narrative_ontology:constraint_beneficiary(coercion_legitimacy_boundary__bodily_autonomy_primary, healthcare_providers).
narrative_ontology:constraint_victim(coercion_legitimacy_boundary__bodily_autonomy_primary, immunocompromised_individuals).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals who decline offered medical interventions such as vaccination or treatment. The constraint categorically shields them from state or institutional coercion, eliminating the threat of forced administration or criminal penalty for refusal. They experience the arrangement as a protective rights boundary.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__bodily_autonomy_primary, medical_refusers, beneficiary,
    moderate, biographical, mobile, national).

% State and institutional actors tasked with population health protection. The categorical consent boundary relieves them of the political, administrative, and moral burden of designing, implementing, and defending coercive medical mandates, while leaving them accountable for outcomes they cannot directly control through coercion.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__bodily_autonomy_primary, public_health_authorities, beneficiary,
    institutional, generational, mobile, national).

% Clinicians and institutions delivering care. The constraint protects them from liability for non-consensual treatment and from state pressure to administer interventions against patient will. They also operationalize the boundary through informed consent protocols and can face sanctions for violations.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__bodily_autonomy_primary, healthcare_providers, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(coercion_legitimacy_boundary__bodily_autonomy_primary, healthcare_providers, agenda_setter).

% Individuals with weakened immune systems who depend on collective compliance or herd immunity for protection against communicable disease. The constraint allows others to refuse preventive measures, externalizing infection risk onto them. They cannot exit their biological vulnerability or the shared epidemiological environment.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__bodily_autonomy_primary, immunocompromised_individuals, payer,
    powerless, biographical, trapped, national).

% Courts and constitutional tribunals that interpret and enforce the categorical consent boundary, striking down medical mandates and adjudicating claims of unauthorized intervention. They define the legal scope and limits of the constraint.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__bodily_autonomy_primary, judiciary, agenda_setter,
    institutional, generational, analytical, national).

% Advocates for mandate-based disease prevention who argue for narrow exceptions to bodily autonomy during outbreaks. They are structurally excluded from the constraint's categorical framing, which treats collective benefit as legally irrelevant to consent requirements.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__bodily_autonomy_primary, public_health_advocates, excluded,
    organized, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes an absolute, legally enforceable boundary around bodily integrity that prevents state and medical institutions from coercing individuals into unwanted interventions, solving the coordination problem of protecting citizens against state medical overreach and non-consensual experimentation.
% TRANSFER_FUNCTION: Transfers the burden of infectious disease risk from the general population and medical refusers to immunocompromised individuals who cannot protect themselves, while transferring political and administrative relief from the mandate-enforcement role to public health authorities.
% ABSENT_VOICES: Immunocompromised individuals are often underrepresented in policy debates about mandate boundaries; their exposure is treated as a secondary effect rather than a primary cost. Public health advocates arguing for proportionate mandate exceptions are sidelined when the boundary is categorical.
% DISAPPEARANCE_RATIONALE: If the categorical consent boundary vanished overnight, states could deploy medical mandates during outbreaks, rearranging the epidemiological risk landscape to concentrate less on immunocompromised populations and shifting the state-citizen relationship toward conditional bodily access.
% FOUNDING_PROBLEM: Historical abuses of medical authority, including forced sterilization, non-consensual experimentation, and coercive state medicine, created demand for an absolute legal barrier to unconsented medical intervention.
% FOUNDING_PROBLEM_CORROBORATION: Human rights organizations and medical ethicists outside the state public health apparatus attest to the ongoing risk of medical coercion; however, immunocompromised patient advocates contest whether a categorical ban is the appropriate remedy, suggesting the founding problem is live but the solution is disputed.
narrative_ontology:disappearance_verdict(coercion_legitimacy_boundary__bodily_autonomy_primary, world_rearranges).
narrative_ontology:founding_problem_status(coercion_legitimacy_boundary__bodily_autonomy_primary, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(coercion_legitimacy_boundary__bodily_autonomy_primary, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(coercion_legitimacy_boundary__bodily_autonomy_primary, 'none', 1).
narrative_ontology:epsilon_provenance(coercion_legitimacy_boundary__bodily_autonomy_primary, 0.48, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(coercion_legitimacy_boundary__bodily_autonomy_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(coercion_legitimacy_boundary__bodily_autonomy_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(coercion_legitimacy_boundary__bodily_autonomy_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48) is moderate because the constraint does not directly confiscate resources but imposes probabilistic health costs on immunocompromised populations through non-enforcement of preventive mandates. Suppression (0.62) is higher than extraction because the categorical boundary requires active legal enforcement to block mandates and penalize violations, suppressing public health alternatives. Theater ratio (0.30) is moderate-low: the Nuremberg-derived norm is substantively functional but partially performative when invoked to avoid cost-benefit analysis. Accessibility collapse (0.58) reflects that mandate alternatives become legally very difficult once the categorical rule is entrenched. Resistance (0.55) captures sustained opposition from public health advocates and immunocompromised communities.
 *
 * PERSPECTIVAL GAP:
 *   Medical refusers experience the constraint as protective rights infrastructure; immunocompromised individuals experience it as exposure infrastructure. Public health authorities experience reduced political and administrative burden, while the judiciary experiences it as a bright-line legal standard. The engine should compute high directionality (near-target) for immunocompromised individuals and low directionality (near-beneficiary) for medical refusers and public health authorities.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (medical_refusers, public_health_authorities, healthcare_providers) are structurally positioned toward the subsidy end: they receive protection from coercion or liability. Victims (immunocompromised_individuals) are structurally trapped (cannot exit biological vulnerability) and bear the externalized risk, placing them near full-target. The judiciary, as agenda_setter with analytical exit, sits near the neutral/administrative end.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint avoids mandatrophy mislabeling because its founding problem (state medical abuse) remains live, but its persistence is not justified solely by that origin. The coordination function (preventing non-consensual intervention) is genuine and separable from the extraction (concentrating disease risk on the vulnerable). It is not a snare because the coordination is not cover; it is not a rope because the harm is asymmetric and not borne by all parties equally; it is not a piton because the function has not atrophied.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Is the categorical bodily autonomy reading a deontological absolute or a contingent legal convention that could be overridden by sufficient collective harm?',
    'Comparative constitutional analysis across jurisdictions with different balances; tracking judicial opinions during public health emergencies.',
    'If contingent, the constraint''s epsilon and classification may shift toward tangled_rope or snare under emergency conditions; if absolute, it remains stable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'The contested nature of the kernel reading''s absolutism').

omega_variable(
    suppression_target_ambiguity,
    'Does the constraint''s suppression fall primarily on state public health capacity or on immunocompromised individuals'' safety?',
    'Epidemiological measurement of disease burden among immunocompromised in jurisdictions with categorical consent boundaries versus proportionate mandate regimes.',
    'If suppression falls mainly on vulnerable populations, the victim set is larger than currently modeled and directionality for immunocompromised rises.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_target_ambiguity, empirical, 'Ambiguity about who bears the primary suppression cost').

omega_variable(
    mandate_enforcer_benefit_mechanism,
    'Do public health authorities genuinely benefit from the constraint''s categorical boundary, or do they suffer reduced capacity that only appears as benefit through avoided political cost?',
    'Ethnographic and administrative analysis of public health agency workload and political accountability in mandate-prohibited versus mandate-permitted jurisdictions.',
    'If the benefit is illusory, the directionality for public_health_authorities shifts toward symmetric or payer, altering chi computation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(mandate_enforcer_benefit_mechanism, empirical, 'Whether mandate enforcers are true beneficiaries or constrained actors').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(coercion_legitimacy_boundary__bodily_autonomy_primary, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(coercion_legitimacy_boundary_bap_tr_t0, coercion_legitimacy_boundary__bodily_autonomy_primary, theater_ratio, 0, 0.2).
narrative_ontology:measurement(coercion_legitimacy_boundary_bap_tr_t8, coercion_legitimacy_boundary__bodily_autonomy_primary, theater_ratio, 8, 0.22).
narrative_ontology:measurement(coercion_legitimacy_boundary_bap_tr_t16, coercion_legitimacy_boundary__bodily_autonomy_primary, theater_ratio, 16, 0.24).
narrative_ontology:measurement(coercion_legitimacy_boundary_bap_tr_t24, coercion_legitimacy_boundary__bodily_autonomy_primary, theater_ratio, 24, 0.26).
narrative_ontology:measurement(coercion_legitimacy_boundary_bap_tr_t32, coercion_legitimacy_boundary__bodily_autonomy_primary, theater_ratio, 32, 0.28).
narrative_ontology:measurement(coercion_legitimacy_boundary_bap_tr_t40, coercion_legitimacy_boundary__bodily_autonomy_primary, theater_ratio, 40, 0.3).

% Extraction over time
narrative_ontology:measurement(coercion_legitimacy_boundary_bap_be_t0, coercion_legitimacy_boundary__bodily_autonomy_primary, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(coercion_legitimacy_boundary_bap_be_t8, coercion_legitimacy_boundary__bodily_autonomy_primary, base_extractiveness, 8, 0.32).
narrative_ontology:measurement(coercion_legitimacy_boundary_bap_be_t16, coercion_legitimacy_boundary__bodily_autonomy_primary, base_extractiveness, 16, 0.35).
narrative_ontology:measurement(coercion_legitimacy_boundary_bap_be_t24, coercion_legitimacy_boundary__bodily_autonomy_primary, base_extractiveness, 24, 0.38).
narrative_ontology:measurement(coercion_legitimacy_boundary_bap_be_t32, coercion_legitimacy_boundary__bodily_autonomy_primary, base_extractiveness, 32, 0.42).
narrative_ontology:measurement(coercion_legitimacy_boundary_bap_be_t40, coercion_legitimacy_boundary__bodily_autonomy_primary, base_extractiveness, 40, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(coercion_legitimacy_boundary_bap_su_t0, coercion_legitimacy_boundary__bodily_autonomy_primary, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(coercion_legitimacy_boundary_bap_su_t8, coercion_legitimacy_boundary__bodily_autonomy_primary, suppression_requirement, 8, 0.42).
narrative_ontology:measurement(coercion_legitimacy_boundary_bap_su_t16, coercion_legitimacy_boundary__bodily_autonomy_primary, suppression_requirement, 16, 0.45).
narrative_ontology:measurement(coercion_legitimacy_boundary_bap_su_t24, coercion_legitimacy_boundary__bodily_autonomy_primary, suppression_requirement, 24, 0.48).
narrative_ontology:measurement(coercion_legitimacy_boundary_bap_su_t32, coercion_legitimacy_boundary__bodily_autonomy_primary, suppression_requirement, 32, 0.52).
narrative_ontology:measurement(coercion_legitimacy_boundary_bap_su_t40, coercion_legitimacy_boundary__bodily_autonomy_primary, suppression_requirement, 40, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(coercion_legitimacy_boundary__bodily_autonomy_primary, enforcement_mechanism).
narrative_ontology:affects_constraint(coercion_legitimacy_boundary__bodily_autonomy_primary, coercion_legitimacy_boundary__public_health_primary).
narrative_ontology:affects_constraint(coercion_legitimacy_boundary__bodily_autonomy_primary, coercion_legitimacy_boundary__proportionality_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the coercion_legitimacy_boundary kernel. The three readings (bodily_autonomy_primary, public_health_primary, proportionality_reading) share a common kernel but instantiate structurally distinct constraints with different beneficiary/victim structures and epsilon profiles.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
