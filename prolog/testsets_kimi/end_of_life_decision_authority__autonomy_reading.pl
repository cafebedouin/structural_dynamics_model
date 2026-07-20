% ============================================================================
% CONSTRAINT STORY: end_of_life_decision_authority__autonomy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_end_of_life_decision_authority__autonomy_reading, []).

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
 *   constraint_id: end_of_life_decision_authority__autonomy_reading
 *   human_readable: End-of-Life Decision Authority â Autonomy Reading
 *   domain: medical ethics/bioethics
 *
 * SUMMARY:
 *   This constraint story models the autonomy reading of end-of-life decision
 *   authority: the normative and legal principle that competent individuals
 *   possess sovereign authority over their own death. It is one reading of a
 *   contested kernel alongside sanctity and vulnerability-protection
 *   readings. Under this reading, healthcare professionals are recast as
 *   facilitators rather than gatekeepers, and authority is centralized in the
 *   competent patient. The constraint coordinates end-of-life care by
 *   resolving who decides, but it also extracts: access is frequently denied
 *   through procedural gatekeeping (prolonging suffering), and the
 *   slippery-slope risk of coercion is externalized onto vulnerable
 *   populations. The structural data treat the constraint as a Tangled
 *   Ropeâgenuine coordination function plus asymmetric extractionâwhile
 *   the metrics independently describe its operation.
 *
 * KEY AGENTS:
 *   - Competent patients (beneficiary; moderate power, constrained exit) â receive nominal sovereignty
 *   - Healthcare professionals (agenda-setter; institutional power, constrained exit) â administer gatekeeping
 *   - Suffering-prolonged patients (payer/victim; powerless, trapped) â denied access despite nominal right
 *   - Vulnerable populations (payer/victim; powerless, trapped) â bear externalized coercion risk
 *   - Disability rights advocates (excluded; organized power) â structurally absent from dominant discourse
 *   - Bioethics commission (observer; institutional power) â analytical oversight seat
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(end_of_life_decision_authority__autonomy_reading, 0.58).
domain_priors:suppression_score(end_of_life_decision_authority__autonomy_reading, 0.65).
domain_priors:theater_ratio(end_of_life_decision_authority__autonomy_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(end_of_life_decision_authority__autonomy_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(end_of_life_decision_authority__autonomy_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(end_of_life_decision_authority__autonomy_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(end_of_life_decision_authority__autonomy_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(end_of_life_decision_authority__autonomy_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(end_of_life_decision_authority__autonomy_reading, tangled_rope).
narrative_ontology:human_readable(end_of_life_decision_authority__autonomy_reading, "End-of-Life Decision Authority â Autonomy Reading").
narrative_ontology:topic_domain(end_of_life_decision_authority__autonomy_reading, "medical ethics/bioethics").

domain_priors:requires_active_enforcement(end_of_life_decision_authority__autonomy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(end_of_life_decision_authority__autonomy_reading, '3fa5b4e2-3f5e-4cfe-bbe5-5d77537e28ce').
narrative_ontology:cs_kernel_codification('3fa5b4e2-3f5e-4cfe-bbe5-5d77537e28ce', formalized).
narrative_ontology:cs_authority_grounding('3fa5b4e2-3f5e-4cfe-bbe5-5d77537e28ce', expertise).
narrative_ontology:cs_interpretation_layer_present('3fa5b4e2-3f5e-4cfe-bbe5-5d77537e28ce').
narrative_ontology:cs_reading_relation('3fa5b4e2-3f5e-4cfe-bbe5-5d77537e28ce', end_of_life_decision_authority__sanctity_reading, coexists_with).
narrative_ontology:cs_reading_relation('3fa5b4e2-3f5e-4cfe-bbe5-5d77537e28ce', end_of_life_decision_authority__vulnerability_protection_reading, influences).
narrative_ontology:cs_axiom('3fa5b4e2-3f5e-4cfe-bbe5-5d77537e28ce', foundational, individual_sovereignty_over_mortality).
narrative_ontology:cs_axiom_status(individual_sovereignty_over_mortality, holdable).
narrative_ontology:cs_axiom_grounding('3fa5b4e2-3f5e-4cfe-bbe5-5d77537e28ce', individual_sovereignty_over_mortality, deontological).
narrative_ontology:cs_axiom('3fa5b4e2-3f5e-4cfe-bbe5-5d77537e28ce', foundational, competency_as_exclusive_legitimacy_condition).
narrative_ontology:cs_axiom_status(competency_as_exclusive_legitimacy_condition, holdable).
narrative_ontology:cs_axiom_grounding('3fa5b4e2-3f5e-4cfe-bbe5-5d77537e28ce', competency_as_exclusive_legitimacy_condition, deontological).
narrative_ontology:cs_reference_frame('3fa5b4e2-3f5e-4cfe-bbe5-5d77537e28ce', patient_sovereignty_frame).
narrative_ontology:cs_drift_state('3fa5b4e2-3f5e-4cfe-bbe5-5d77537e28ce', contemporary_medical_practice, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('3fa5b4e2-3f5e-4cfe-bbe5-5d77537e28ce', '').
narrative_ontology:cs_kernel_id(end_of_life_decision_authority__autonomy_reading, end_of_life_decision_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(end_of_life_decision_authority__autonomy_reading, competent_patients).
narrative_ontology:constraint_victim(end_of_life_decision_authority__autonomy_reading, suffering_prolonged_patients).
narrative_ontology:constraint_victim(end_of_life_decision_authority__autonomy_reading, vulnerable_populations).
narrative_ontology:constraint_vindicates(end_of_life_decision_authority__autonomy_reading, autonomy_principle).
narrative_ontology:constraint_vindicates(end_of_life_decision_authority__autonomy_reading, informed_consent_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Possess nominal sovereign authority over end-of-life decisions, including refusal of treatment and access to assisted dying where legal. Must navigate competency assessments, waiting periods, and eligibility criteria that often delay or deny access; exit is constrained by medical gatekeeping even where the right is legally established.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__autonomy_reading, competent_patients, beneficiary,
    moderate, biographical, constrained, national).

% Serve as facilitators under this reading, assessing competency and administering aid-in-dying protocols. They control the practical gatekeeping of autonomy through diagnosis, prognosis, and referral networks. Their role is redefined from paternalistic decider to procedural facilitator, but they retain effective veto power through non-participation or institutional policy.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__autonomy_reading, healthcare_professionals, agenda_setter,
    institutional, biographical, constrained, national).

% Are formally competent but structurally denied access to death due to eligibility exclusions, institutional resistance, procedural delays, or conscientious objection. Bear the cost of the constraintâs operation through prolonged, unwanted suffering despite the nominal autonomy framework.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__autonomy_reading, suffering_prolonged_patients, payer,
    powerless, biographical, trapped, national).

% Bear the externalized slippery-slope risk of the autonomy framework; face potential familial, economic, or systemic pressure to choose death due to resource scarcity or perceived burden. Lack effective voice in the policy design that nominally empowers individuals.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__autonomy_reading, vulnerable_populations, payer,
    powerless, generational, trapped, national).

% Argue that autonomy frameworks devalue disabled and dependent life and create perverse incentives to end life prematurely. Largely excluded from the dominant medical-ethics discourse that frames the issue as a simple conflict between individual choice and institutional paternalism.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__autonomy_reading, disability_rights_advocates, excluded,
    organized, generational, constrained, national).

% Monitors implementation and produces guidelines on end-of-life practice. Occupies an analytical seat without direct extraction or payment, though its expertise is frequently cited by agenda-setters to legitimize the autonomy framework.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__autonomy_reading, bioethics_commission, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves the collective-action problem of end-of-life decision-making by assigning unambiguous authority to the competent individual patient, eliminating conflict among family members, physicians, and institutions over who decides.
% TRANSFER_FUNCTION: Moves decision-making authority from institutional gatekeepers and family units to the competent individual; moves the burden of justification from proving a right to die to demonstrating competency.
% ABSENT_VOICES: Disability rights advocates who view autonomy frameworks as threats to vulnerable life; sanctity-oriented religious authorities who reject intentional life-ending; family members who retain emotional and relational stakes but lose formal standing.
% DISAPPEARANCE_RATIONALE: If the autonomy reading vanished, healthcare professionals and institutions would reclaim decision-making authority; advance directives and informed-consent regimes would revert to paternalistic or best-interest standards; competent patients would lose the standing to demand withdrawal of treatment or assisted dying.
% FOUNDING_PROBLEM: Traditional medical paternalism in which physicians, institutions, or families made end-of-life decisions for patients, frequently prolonging suffering and disregarding patient preferences.
% FOUNDING_PROBLEM_CORROBORATION: Patient-advocacy organizations and critical historians of medicine attest the paternalistic problem from outside the benefiting parties; disability-rights organizations and some palliative-care physicians contest that the autonomy framework solves it without creating new harms, corroborating the contested status from alternative seats.
narrative_ontology:disappearance_verdict(end_of_life_decision_authority__autonomy_reading, world_rearranges).
narrative_ontology:founding_problem_status(end_of_life_decision_authority__autonomy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(end_of_life_decision_authority__autonomy_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(end_of_life_decision_authority__autonomy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(end_of_life_decision_authority__autonomy_reading, 0.58, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(end_of_life_decision_authority__autonomy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(end_of_life_decision_authority__autonomy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(end_of_life_decision_authority__autonomy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) is moderate-to-high because, although the framework nominally empowers patients, the same institutional machinery that grants authority also withholds it through eligibility restrictions, competency disputes, and conscientious objection. Suppression (0.65) reflects the active enforcement required to maintain patient sovereignty against persistent paternalistic, sanctity-based, and institutional alternatives; it also measures the suppression of family veto and religious objection. Theater ratio (0.25) is moderate-low: competency assessments perform real cognitive screening but increasingly function as bureaucratic delay. Resistance (0.75) is high because disability-rights groups, sanctity-oriented institutions, and conscientious objectors actively contest the framework. Accessibility collapse (0.40) is moderate: alternatives (paternalistic best-interest standards, sanctity-based refusal) remain available in parallel institutions and jurisdictions.
 *
 * PERSPECTIVAL GAP:
 *   From the competent-patient seat, the constraint reads as sovereignty and liberation. From the suffering-prolonged seat, it reads as a false promiseâauthority granted in name but denied in practice through procedural obstacles. From the vulnerable-population seat, it reads as exposure to covert social pressure and resource-driven coercion. The engine computes these divergent seat classifications from the same structural data without reconciling them.
 *
 * DIRECTIONALITY LOGIC:
 *   Competent patients are declared beneficiaries (low d, subsidized by the constraintâs grant of authority). Suffering-prolonged patients and vulnerable populations are declared victims/payers (high d, extraction amplified). Healthcare professionals are agenda-setters who administer the constraint; they are neither beneficiaries nor victims in the base properties, so their directionality falls to the canonical fallback for institutional power with constrained exit. Disability advocates are excluded from the beneficiary set and sit outside the primary transfer.
 *
 * MANDATROPHY ANALYSIS:
 *   Without the victim declarations (suffering-prolonged, vulnerable populations), the autonomy reading would present as a Ropeâpure coordination of decision-making. Without the beneficiary declaration (competent patients), it would present as a Snareâinstitutional domination masquerading as empowerment. The Tangled Rope classification is warranted only because both the coordination function (clear authority assignment) and the asymmetric extraction (prolonged suffering, externalized risk) are structurally present and actively enforced.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    slippery_slope_empirical_status,
    'Does the autonomy reading produce measurable coercion or mortality pressure on vulnerable populations, or does the slippery slope remain hypothetical?',
    'Longitudinal population studies comparing mortality and self-reported pressure in jurisdictions with and without broad autonomy frameworks.',
    'If empirically confirmed, vulnerable_populations victim status strengthens and extractiveness rises; if unsupported, the victim claim weakens and the constraint edges toward Rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(slippery_slope_empirical_status, empirical, 'Empirical status of autonomy slippery slope risk').

omega_variable(
    competency_gatekeeping_function,
    'Do competency assessments and waiting periods function as legitimate coordination safeguards or as covert suppression mechanisms that prolong suffering?',
    'Comparative outcome analysis measuring time-to-access and rate-of-denial against clinical necessity and patient-reported distress.',
    'If gatekeeping is primarily suppressive, theater_ratio and extractiveness increase; if primarily protective, the coordination function dominates and the constraint may recertify toward Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(competency_gatekeeping_function, empirical, 'Competency assessment as safeguard or suppression').

omega_variable(
    autonomy_sanctity_logical_relation,
    'Can the autonomy reading and the sanctity reading coexist within a single bioethical commitment framework, or are they mutually exclusive at the foundational level?',
    'Conceptual analysis of jurisdictions and ethical systems that attempt synthetic balancing versus those that adopt pure autonomy or pure sanctity.',
    'If mutually exclusive, the reading_relations entry for sanctity should upgrade to forecloses; if synthesizable, coexists_with is correct and the autonomy reading is less absolute than its axioms suggest.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(autonomy_sanctity_logical_relation, conceptual, 'Logical relation between autonomy and sanctity readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(end_of_life_decision_authority__autonomy_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eol_autonomy_tr_t0, end_of_life_decision_authority__autonomy_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(eol_autonomy_tr_t10, end_of_life_decision_authority__autonomy_reading, theater_ratio, 10, 0.12).
narrative_ontology:measurement(eol_autonomy_tr_t20, end_of_life_decision_authority__autonomy_reading, theater_ratio, 20, 0.15).
narrative_ontology:measurement(eol_autonomy_tr_t30, end_of_life_decision_authority__autonomy_reading, theater_ratio, 30, 0.18).
narrative_ontology:measurement(eol_autonomy_tr_t40, end_of_life_decision_authority__autonomy_reading, theater_ratio, 40, 0.22).
narrative_ontology:measurement(eol_autonomy_tr_t50, end_of_life_decision_authority__autonomy_reading, theater_ratio, 50, 0.25).

% Extraction over time
narrative_ontology:measurement(eol_autonomy_be_t0, end_of_life_decision_authority__autonomy_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(eol_autonomy_be_t10, end_of_life_decision_authority__autonomy_reading, base_extractiveness, 10, 0.38).
narrative_ontology:measurement(eol_autonomy_be_t20, end_of_life_decision_authority__autonomy_reading, base_extractiveness, 20, 0.45).
narrative_ontology:measurement(eol_autonomy_be_t30, end_of_life_decision_authority__autonomy_reading, base_extractiveness, 30, 0.5).
narrative_ontology:measurement(eol_autonomy_be_t40, end_of_life_decision_authority__autonomy_reading, base_extractiveness, 40, 0.54).
narrative_ontology:measurement(eol_autonomy_be_t50, end_of_life_decision_authority__autonomy_reading, base_extractiveness, 50, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(eol_autonomy_su_t0, end_of_life_decision_authority__autonomy_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(eol_autonomy_su_t10, end_of_life_decision_authority__autonomy_reading, suppression_requirement, 10, 0.45).
narrative_ontology:measurement(eol_autonomy_su_t20, end_of_life_decision_authority__autonomy_reading, suppression_requirement, 20, 0.52).
narrative_ontology:measurement(eol_autonomy_su_t30, end_of_life_decision_authority__autonomy_reading, suppression_requirement, 30, 0.58).
narrative_ontology:measurement(eol_autonomy_su_t40, end_of_life_decision_authority__autonomy_reading, suppression_requirement, 40, 0.62).
narrative_ontology:measurement(eol_autonomy_su_t50, end_of_life_decision_authority__autonomy_reading, suppression_requirement, 50, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(end_of_life_decision_authority__autonomy_reading, end_of_life_decision_authority__sanctity_reading).
narrative_ontology:affects_constraint(end_of_life_decision_authority__autonomy_reading, end_of_life_decision_authority__vulnerability_protection_reading).

% DUAL FORMULATION NOTE:
% This constraint is one component of the end_of_life_decision_authority kernel. The kernel decomposes into three structurally distinct readingsâautonomy, sanctity, and vulnerability-protectionâeach with distinct epsilon values, beneficiary/victim structures, and normative premises. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
