% ============================================================================
% CONSTRAINT STORY: vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, []).

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
 *   constraint_id: vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading
 *   human_readable: Vaccine Mandate Legitimacy â Bodily Autonomy Primacy Reading
 *   domain: public_health_policy/constitutional_law/bioethics
 *
 * SUMMARY:
 *   This constraint story captures the bodily autonomy primacy reading of the
 *   vaccine mandate legitimacy kernel. Under this reading, state-imposed
 *   vaccination mandates constitute an illegitimate coercive extraction of
 *   medical self-sovereignty, categorically impermissible regardless of
 *   epidemiological outcome. The standing arrangement under contest is the
 *   mandate regime itself â not a hypothetical rights-respecting
 *   alternative. Immunocompromised individuals enter the victim set because
 *   the regime pressures them toward medically unsafe compliance or excludes
 *   them without reducing their objective risk; vulnerable populations bear
 *   residual exposure risk in environments where mandate-driven relaxation of
 *   non-pharmaceutical measures substitutes vaccination coverage for actual
 *   safety. Liberty advocacy movements are structural beneficiaries of the
 *   constraint's existence, gaining mobilization and political capital from
 *   the contest it generates. The reading is deontological and absolute; it
 *   acknowledges the coordination function of public health (disease
 *   prevention) but holds that bodily integrity cannot be traded against it.
 *
 * KEY AGENTS:
 *   - liberty_advocacy_movements: Primary beneficiary (organized/mobile) â gains political capital and membership from opposing the mandate regime.
 *   - immunocompromised_population: Primary target (powerless/trapped) â cannot safely comply and cannot exit the risk environment created by mandate-driven policy.
 *   - general_population_subject_to_mandate: Secondary target (moderate/constrained) â faces employment and social exclusion for non-compliance.
 *   - high_risk_vulnerable_population: Tertiary target (powerless/trapped) â bears breakthrough exposure risk when mandates substitute for layered precautions.
 *   - public_health_authorities: Agenda setter (institutional/analytical) â designs, justifies, and enforces the mandate framework.
 *   - healthcare_systems: Co-beneficiary (institutional/constrained) â receives reduced acute load from population immunization but does not control mandate design.
 *   - constitutional_judiciary: Analytical observer (institutional/analytical) â adjudicates legitimacy claims without direct cost or benefit.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, 0.82).
domain_priors:suppression_score(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, 0.78).
domain_priors:theater_ratio(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, 0.5).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, theater_ratio, 0.5).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, tangled_rope).
narrative_ontology:human_readable(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, "Vaccine Mandate Legitimacy â Bodily Autonomy Primacy Reading").
narrative_ontology:topic_domain(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, "public_health_policy/constitutional_law/bioethics").

domain_priors:requires_active_enforcement(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, '5511e46e-2aac-4263-8b45-ce368af1f4ca').
narrative_ontology:cs_kernel_codification('5511e46e-2aac-4263-8b45-ce368af1f4ca', fixed_text).
narrative_ontology:cs_authority_grounding('5511e46e-2aac-4263-8b45-ce368af1f4ca', lineage).
narrative_ontology:cs_interpretation_layer_present('5511e46e-2aac-4263-8b45-ce368af1f4ca').
narrative_ontology:cs_reading_relation('5511e46e-2aac-4263-8b45-ce368af1f4ca', vaccine_mandate_legitimacy__public_health_primacy_reading, forecloses).
narrative_ontology:cs_reading_relation('5511e46e-2aac-4263-8b45-ce368af1f4ca', vaccine_mandate_legitimacy__risk_stratification_reading, forecloses).
narrative_ontology:cs_axiom('5511e46e-2aac-4263-8b45-ce368af1f4ca', foundational, medical_self_sovereignty_absolute).
narrative_ontology:cs_axiom_status(medical_self_sovereignty_absolute, holdable).
narrative_ontology:cs_axiom_grounding('5511e46e-2aac-4263-8b45-ce368af1f4ca', medical_self_sovereignty_absolute, deontological).
narrative_ontology:cs_axiom('5511e46e-2aac-4263-8b45-ce368af1f4ca', foundational, state_coercion_medically_impermissible).
narrative_ontology:cs_axiom_status(state_coercion_medically_impermissible, holdable).
narrative_ontology:cs_axiom_grounding('5511e46e-2aac-4263-8b45-ce368af1f4ca', state_coercion_medically_impermissible, deontological).
narrative_ontology:cs_reference_frame('5511e46e-2aac-4263-8b45-ce368af1f4ca', bodily_autonomy_constitutional_settlement).
narrative_ontology:cs_drift_state('5511e46e-2aac-4263-8b45-ce368af1f4ca', post_pandemic_mandate_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('5511e46e-2aac-4263-8b45-ce368af1f4ca', '').
narrative_ontology:cs_kernel_id(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, vaccine_mandate_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, liberty_advocacy_movements).
narrative_ontology:constraint_beneficiary(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, healthcare_systems).
narrative_ontology:constraint_victim(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, immunocompromised_population).
narrative_ontology:constraint_victim(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, general_population_subject_to_mandate).
narrative_ontology:constraint_victim(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, high_risk_vulnerable_population).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gain membership, funding, and political influence by opposing the mandate regime. They organize protests, fund legal challenges, and frame bodily autonomy as foundational. Their organizational vitality is partially contingent on the persistence of the mandate they oppose; if mandates vanished, their current mobilization axis would dissipate, though they could pivot to other issues.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, liberty_advocacy_movements, beneficiary,
    organized, generational, mobile, national).

% Cannot mount adequate immune responses to available vaccines, yet face elevated infection risk in mandate-driven environments that assume vaccination equals safety. Often denied meaningful exemptions, excluded from workplaces requiring proof of vaccination, or pressured to undergo procedures offering limited protection with real risks. Cannot exit their medical vulnerability or the jurisdiction without severe life disruption.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, immunocompromised_population, payer,
    powerless, immediate, trapped, national).

% Individuals facing loss of employment, educational exclusion, or social marginalization if they refuse vaccination. They experience direct state coercion overriding their medical decision-making. Exit options are limited to compliance, acceptance of penalties, or geographic relocation, all imposing substantial costs.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, general_population_subject_to_mandate, payer,
    moderate, biographical, constrained, national).

% Elderly and comorbid individuals who remain vulnerable to breakthrough infection despite vaccine-derived population immunity. They bear residual exposure risk in environments where non-pharmaceutical precautions are relaxed under the assumption that vaccination has solved the collective problem.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, high_risk_vulnerable_population, payer,
    powerless, immediate, trapped, national).

% Design and enforce vaccination mandates using epidemiological data and legal authority. They set exemption criteria, monitor compliance rates, and defend the mandate as necessary for collective welfare. They can revise or abandon the mandate framework but are institutionally committed to its legitimacy.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, public_health_authorities, agenda_setter,
    institutional, generational, analytical, national).

% Receive reduced acute patient loads and lower nosocomial transmission when mandates increase population vaccination coverage. They benefit from the coordination function but do not set mandate terms. They are constrained by regulatory requirements to participate in enforcement, reporting, and verification.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, healthcare_systems, beneficiary,
    institutional, biographical, constrained, national).

% Adjudicates challenges to mandate authority, weighing bodily autonomy claims against state police power. Their rulings determine enforceability but they neither benefit from nor pay the direct costs of the constraint.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, constitutional_judiciary, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preventing infectious disease transmission and protecting healthcare capacity through uniform population immunization, solving the free-rider problem in vaccination where individual protection alone leaves collective gaps.
% TRANSFER_FUNCTION: Moves bodily compliance, political mobilization, and moral legitimacy from the general population and vulnerable groups to the state enforcement apparatus, while simultaneously generating mobilization resources for liberty advocacy movements and operational relief for healthcare systems.
% ABSENT_VOICES: Individuals with medical contraindications who are denied exemptions under narrow criteria; religious objectors excluded from secular exemption frameworks; healthcare workers opposing mandates but fearing professional retaliation; alternative public health strategists advocating focused protection or non-pharmaceutical interventions who are marginalized once mandate frameworks dominate policy.
% DISAPPEARANCE_RATIONALE: If vaccine mandates disappeared, public health authorities would reorganize around persuasion and non-pharmaceutical measures; workplaces and schools would abandon exclusion policies; the political energy fueling liberty advocacy movements would dissipate or redirect; vulnerable populations would face a recalculated exposure environment without mandate-driven relaxation of layered precautions.
% FOUNDING_PROBLEM: Controlling epidemic disease when voluntary vaccination rates are insufficient to prevent healthcare system collapse or widespread mortality, and individual vaccination decisions generate negative externalities for the collective.
% FOUNDING_PROBLEM_CORROBORATION: Independent epidemiologists and historians of public health attest to the genuine threat of uncontrolled epidemic disease. Constitutional scholars and patient advocacy groups unaffiliated with liberty movements attest that the mandate regime disproportionately harms the immunocompromised and shifts burden rather than solving the collective problem.
narrative_ontology:disappearance_verdict(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, 0.82, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.82) because the constraint extracts bodily autonomy itself â a foundational sovereignty â through threatened exclusion from economic and social life. Suppression is high (0.78) because the mandate regime actively suppresses non-compliance via employment termination, educational exclusion, and digital verification systems. Theater ratio is substantial (0.50) because a growing share of mandate enforcement performs compliance rather than achieving marginal public health benefit, particularly as variants evolve and transmission dynamics shift. Accessibility collapse (0.65) reflects the marginalization of alternative strategies (focused protection, routine testing, natural immunity accommodation) once the mandate framework dominates policy imagination. Resistance (0.72) is high due to sustained legal, political, and civil resistance across multiple jurisdictions. The temporal series show extraction and theater rising through the interval as emergency framing gave way to institutionalized enforcement, then plateauing as the constraint normalized.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (public health authorities) experiences the constraint as legitimate coordination solving a collective-action problem; from this seat the metrics would read as moderate extraction for substantial collective benefit. The payer seats (immunocompromised, general population, vulnerable) experience the same structure as coercive extraction of bodily sovereignty with asymmetrically distributed harms. The beneficiary seat (liberty advocacy) experiences the constraint as the necessary condition for its own mobilization. The engine computes this divergence from the structural data â powerless/trapped agents near full-target directionality, institutional/arbitrage agents near beneficiary end.
 *
 * DIRECTIONALITY LOGIC:
 *   Public health authorities sit near the low-directionality end as administrators who could change the policy. Liberty advocacy movements are beneficiaries of the constraint's existence (it generates their mobilization substrate) and have mobile exit, placing them at low directionality. Immunocompromised and vulnerable populations are trapped with no exit, placing them at high directionality. The general population is constrained â exit is possible but costly â yielding intermediate-high directionality. No overrides are needed: the structural derivation (beneficiary/victim declarations plus exit atoms) captures the actual relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â uncontrolled epidemic threatening healthcare collapse â was live at the constraint's origin. The bodily autonomy reading contests whether the mandate actually solves this problem or merely displaces its burden onto vulnerable populations. If the founding problem has shifted from acute emergency to endemic management, the constraint risks mandatrophy: it persists as an enforcement mechanism beyond its coordinating justification. The temporal measurements show base_extractiveness rising while the founding threat receded, suggesting accumulation of extraction atop a coordination function that may have expired â a drift pattern consistent with tangled rope sliding toward snare. The reading prevents mislabeling by insisting that even a live founding problem cannot justify this constraint, preserving the deontological classification boundary.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    bodily_autonomy_kernel_contest,
    'Does the absolute bodily autonomy axiom foreclose all mandate frameworks, or can it coexist with risk-stratified targeted mandates?',
    'Jurisprudential analysis of whether deontological bodily autonomy claims logically entail prohibition of all state medical coercion, or leave room for proportionate, narrowly tailored interventions.',
    'If absolute autonomy forecloses all mandates, the constraint is structurally a snare from this reading; if targeted mandates survive, the constraint family decomposes further.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(bodily_autonomy_kernel_contest, conceptual, 'Scope of the bodily autonomy axiom relative to targeted public health coercion.').

omega_variable(
    immunocompromised_victim_status,
    'Are immunocompromised individuals genuinely victims of the mandate regime, or does the regime incidentally fail to protect them while primarily benefiting them through herd immunity?',
    'Empirical measurement of infection and adverse outcome rates among immunocompromised populations under mandate regimes versus voluntary regimes, combined with qualitative analysis of exemption accessibility and social exclusion.',
    'If immunocompromised populations are net beneficiaries, the victim set shrinks and directionality shifts; if they are net victims, the extraction profile is more severe.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(immunocompromised_victim_status, empirical, 'Whether immunocompromised populations bear net costs from mandate regimes.').

omega_variable(
    founding_problem_obsolescence,
    'Has the founding epidemic threat that justified the mandate regime subsided to the point where the constraint persists beyond its coordinating function?',
    'Epidemiological surveillance data, mortality and morbidity trends, and healthcare capacity metrics over the constraint''s interval.',
    'If the founding problem is dead, the constraint is in mandatrophy and should compute as piton; if live, it remains tangled rope or snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_obsolescence, empirical, 'Whether the mandate regime''s founding epidemic threat remains active.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, 0, 48).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vmbapr_tr_t0, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(vmbapr_tr_t8, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, theater_ratio, 8, 0.3).
narrative_ontology:measurement(vmbapr_tr_t16, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, theater_ratio, 16, 0.38).
narrative_ontology:measurement(vmbapr_tr_t24, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, theater_ratio, 24, 0.45).
narrative_ontology:measurement(vmbapr_tr_t32, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, theater_ratio, 32, 0.5).
narrative_ontology:measurement(vmbapr_tr_t40, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, theater_ratio, 40, 0.52).
narrative_ontology:measurement(vmbapr_tr_t48, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, theater_ratio, 48, 0.5).

% Extraction over time
narrative_ontology:measurement(vmbapr_be_t0, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(vmbapr_be_t8, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, base_extractiveness, 8, 0.62).
narrative_ontology:measurement(vmbapr_be_t16, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, base_extractiveness, 16, 0.7).
narrative_ontology:measurement(vmbapr_be_t24, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, base_extractiveness, 24, 0.76).
narrative_ontology:measurement(vmbapr_be_t32, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, base_extractiveness, 32, 0.8).
narrative_ontology:measurement(vmbapr_be_t40, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, base_extractiveness, 40, 0.82).
narrative_ontology:measurement(vmbapr_be_t48, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, base_extractiveness, 48, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(vmbapr_su_t0, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(vmbapr_su_t8, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, suppression_requirement, 8, 0.65).
narrative_ontology:measurement(vmbapr_su_t16, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, suppression_requirement, 16, 0.75).
narrative_ontology:measurement(vmbapr_su_t24, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, suppression_requirement, 24, 0.8).
narrative_ontology:measurement(vmbapr_su_t32, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, suppression_requirement, 32, 0.82).
narrative_ontology:measurement(vmbapr_su_t40, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, suppression_requirement, 40, 0.8).
narrative_ontology:measurement(vmbapr_su_t48, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, suppression_requirement, 48, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, vaccine_mandate_legitimacy__public_health_primacy_reading).
narrative_ontology:affects_constraint(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, vaccine_mandate_legitimacy__risk_stratification_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the vaccine_mandate_legitimacy kernel. The kernel decomposes into at least three structurally distinct constraints: bodily_autonomy_primacy_reading (absolute prohibition of medical coercion), public_health_primacy_reading (state authority to mandate for collective welfare), and risk_stratification_reading (proportional coercion contingent on actuarial thresholds). Each reading carries a distinct epsilon, beneficiary/victim structure, and normative axiom set. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
