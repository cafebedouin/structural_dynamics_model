% ============================================================================
% CONSTRAINT STORY: end_of_life_decision_authority__autonomy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
 *   constraint_id: end_of_life_decision_authority__autonomy_reading
 *   human_readable: Patient Sovereignty in End-of-Life Decisions (Autonomy Reading)
 *   domain: medical_ethics/bioethics/end_of_life_policy
 *
 * SUMMARY:
 *   This constraint story instantiates the autonomy reading of the
 *   end-of-life decision authority kernel: the claim that competent
 *   individuals possess sovereign authority over their own death,
 *   operationalized through assisted dying and advance directive legal
 *   frameworks. Under this reading, the standing arrangement is a
 *   medico-legal authority structure that centralizes end-of-life decision
 *   power in the competent patient. The structural delta identifies
 *   suffering-prolonged patients as victims when access is denied, healthcare
 *   professionals as facilitators bearing role-transformation costs, and
 *   slippery slope risks as externalized to vulnerable populations. The story
 *   is authored as a tangled_rope because genuine coordination (resolving
 *   decisional conflict, honoring self-determination) coexists with
 *   asymmetric extraction (patients denied access by procedural gatekeeping,
 *   professionals compelled to participate against conscience, vulnerable
 *   groups bearing uncompensated risk).
 *
 * KEY AGENTS:
 *   - competent_patients (beneficiary/moderate/constrained): Receive sovereignty but depend on institutional gatekeeping
 *   - suffering_prolonged_patients (payer/powerless/trapped): Denied access, bear prolonged suffering
 *   - healthcare_professionals (payer/organized/constrained): Transformed into facilitators, bear conscience and identity costs
 *   - legislatures_and_medical_boards (agenda_setter/institutional/constrained): Administer gatekeeping and eligibility criteria
 *   - disability_rights_advocates (excluded/organized/mobile): Warn of systemic coercion, structurally marginalized in autonomy framing
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(end_of_life_decision_authority__autonomy_reading, 0.58).
domain_priors:suppression_score(end_of_life_decision_authority__autonomy_reading, 0.52).
domain_priors:theater_ratio(end_of_life_decision_authority__autonomy_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(end_of_life_decision_authority__autonomy_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(end_of_life_decision_authority__autonomy_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(end_of_life_decision_authority__autonomy_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(end_of_life_decision_authority__autonomy_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(end_of_life_decision_authority__autonomy_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(end_of_life_decision_authority__autonomy_reading, tangled_rope).
narrative_ontology:human_readable(end_of_life_decision_authority__autonomy_reading, "Patient Sovereignty in End-of-Life Decisions (Autonomy Reading)").
narrative_ontology:topic_domain(end_of_life_decision_authority__autonomy_reading, "medical_ethics/bioethics/end_of_life_policy").

domain_priors:requires_active_enforcement(end_of_life_decision_authority__autonomy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(end_of_life_decision_authority__autonomy_reading, 'd9e6d8ab-b4f7-4fa4-ac96-d07ca9351577').
narrative_ontology:cs_kernel_codification('d9e6d8ab-b4f7-4fa4-ac96-d07ca9351577', formalized).
narrative_ontology:cs_authority_grounding('d9e6d8ab-b4f7-4fa4-ac96-d07ca9351577', lineage).
narrative_ontology:cs_interpretation_layer_present('d9e6d8ab-b4f7-4fa4-ac96-d07ca9351577').
narrative_ontology:cs_reading_relation('d9e6d8ab-b4f7-4fa4-ac96-d07ca9351577', end_of_life_decision_authority__sanctity_reading, coexists_with).
narrative_ontology:cs_reading_relation('d9e6d8ab-b4f7-4fa4-ac96-d07ca9351577', end_of_life_decision_authority__vulnerability_protection_reading, influences).
narrative_ontology:cs_axiom('d9e6d8ab-b4f7-4fa4-ac96-d07ca9351577', foundational, competent_individual_sovereignty_over_death).
narrative_ontology:cs_axiom_status(competent_individual_sovereignty_over_death, holdable).
narrative_ontology:cs_axiom_grounding('d9e6d8ab-b4f7-4fa4-ac96-d07ca9351577', competent_individual_sovereignty_over_death, deontological).
narrative_ontology:cs_axiom('d9e6d8ab-b4f7-4fa4-ac96-d07ca9351577', foundational, suffering_as_justification_for_life_ending).
narrative_ontology:cs_axiom_status(suffering_as_justification_for_life_ending, holdable).
narrative_ontology:cs_axiom_grounding('d9e6d8ab-b4f7-4fa4-ac96-d07ca9351577', suffering_as_justification_for_life_ending, deontological).
narrative_ontology:cs_reference_frame('d9e6d8ab-b4f7-4fa4-ac96-d07ca9351577', patient_sovereignty_ideal).
narrative_ontology:cs_drift_state('d9e6d8ab-b4f7-4fa4-ac96-d07ca9351577', contemporary_medical_practice, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('d9e6d8ab-b4f7-4fa4-ac96-d07ca9351577', '').
narrative_ontology:cs_kernel_id(end_of_life_decision_authority__autonomy_reading, end_of_life_decision_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(end_of_life_decision_authority__autonomy_reading, competent_patients).
narrative_ontology:constraint_beneficiary(end_of_life_decision_authority__autonomy_reading, patient_advocacy_groups).
narrative_ontology:constraint_victim(end_of_life_decision_authority__autonomy_reading, suffering_prolonged_patients).
narrative_ontology:constraint_victim(end_of_life_decision_authority__autonomy_reading, healthcare_professionals).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Are granted legal authority to refuse treatment and to request medical assistance in dying subject to competence assessments, waiting periods, and eligibility criteria. They gain control over the timing and manner of death but remain dependent on institutional gatekeepers to validate and execute their choices.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__autonomy_reading, competent_patients, beneficiary,
    moderate, biographical, constrained, national).

% Patients who meet suffering criteria but are denied access to assisted dying because of procedural delays, restrictive eligibility categories, institutional conscientious objection, or geographic inequity. They bear the direct cost of prolonged suffering that the autonomy framework promises but fails to deliver.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__autonomy_reading, suffering_prolonged_patients, payer,
    powerless, immediate, trapped, national).

% Are transformed by law and institutional policy from life-preserving healers into facilitators of patient-directed death. Must assess competence, provide referrals, or directly administer life-ending procedures even when these acts conflict with professional identity, conscience, or traditional ethics. Professional licensure and employment conditions constrain refusal.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__autonomy_reading, healthcare_professionals, payer,
    organized, biographical, constrained, national).

% Draft eligibility criteria, competence standards, waiting periods, and reporting requirements. They administer the gatekeeping function that determines which patient requests are honored and which are denied, and they enforce compliance against institutions or professionals that resist.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__autonomy_reading, legislatures_and_medical_boards, agenda_setter,
    institutional, generational, constrained, national).

% Advance and defend the legal and social architecture of patient sovereignty. They benefit from the institutionalization of autonomy because it validates their organizational mission and provides a clear policy victory, even as they remain critical of access barriers.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__autonomy_reading, patient_advocacy_groups, beneficiary,
    organized, generational, mobile, national).

% Argue that autonomy frameworks expose disabled and vulnerable people to systemic coercion, devaluation, and slippery-slope erosion of protective standards. Their perspective is structurally marginalized in policy design because the competence framework treats end-of-life decisions as individual rather than socially embedded.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__autonomy_reading, disability_rights_advocates, excluded,
    organized, generational, mobile, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves end-of-life decision-making conflicts by establishing a single, unambiguous locus of authority in the competent patient, eliminating disputes among family members, physicians, and institutions over who decides when life ends and under what conditions.
% TRANSFER_FUNCTION: Moves decisional authority from families, physicians, and institutional ethics committees to the individual patient; moves the burden of assessment, referral, and administration to healthcare professionals; moves the risk of systemic coercion and devaluation to disabled and vulnerable populations who did not choose to enter the regime.
% ABSENT_VOICES: Disability rights advocates who argue that autonomy frameworks mask systemic coercion and that competence assessments fail to detect social pressure; also religious and traditional communities who hold that end-of-life authority is communal or divine rather than individual.
% DISAPPEARANCE_RATIONALE: If patient sovereignty over end-of-life decisions vanished overnight, medical practice would revert to physician paternalism and family-driven decision-making, end-of-life law would collapse into ambiguity, and the bioethics infrastructure built around autonomy would reorganize around alternative principles such as sanctity or vulnerability protection.
% FOUNDING_PROBLEM: Historical paternalism in medicine and family dynamics frequently overrode the wishes of competent individuals, subjecting them to unwanted life-extending treatment and depriving them of control over their own dying process.
% FOUNDING_PROBLEM_CORROBORATION: Patient advocacy organizations and liberal bioethicists attest the problem persists where autonomy is denied. Disability rights advocates and some palliative care physicians attest the founding problem has been superseded by the new problems of coercion, inadequate safeguards, and procedural gatekeeping; they speak from outside the direct beneficiary set.
narrative_ontology:disappearance_verdict(end_of_life_decision_authority__autonomy_reading, world_rearranges).
narrative_ontology:founding_problem_status(end_of_life_decision_authority__autonomy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(end_of_life_decision_authority__autonomy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
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
 *   Extractiveness (0.58) reflects the asymmetric costs: genuine autonomy for some, prolonged suffering for others denied access, and professional conscience burdens. Suppression (0.52) captures the active suppression of physician paternalism, family veto, and sanctity-based objections required to maintain patient sovereignty. Theater_ratio (0.40) acknowledges that competence assessments and waiting periods function partly as performative safeguards that generate bureaucratic delay without proportional protection. Accessibility_collapse (0.45) reflects that alternatives (family-centered decision, physician-directed care) are legally and ethically marginalized but not fully extinguished. Resistance (0.50) registers persistent opposition from medical, disability, and religious communities.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter and beneficiary seats (legislatures, competent patients) experience the constraint as legitimate coordination that resolves tragic conflicts. The payer seats (denied patients, professionals) experience it as a structure that extracts their suffering or professional identity. The engine computes this divergence from the structural data; the autonomy reading does not resolve it but instantiates one side of it.
 *
 * DIRECTIONALITY LOGIC:
 *   Competent patients and advocacy groups sit at the beneficiary end (d near 0.0): the constraint subsidizes their decisional authority. Suffering-prolonged patients sit at the full-target end (d near 1.0): the same structure that empowers others traps them in prolonged suffering. Healthcare professionals are mid-range targets (d ~0.65): the constraint extracts traditional professional identity and substitutes a facilitation role they did not choose. Legislatures and boards are agenda-setters with low d (they control and benefit from the administrative function). Disability rights advocates are excluded observers with high d relative to the risk externalized onto their constituency.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â medical paternalism overriding patient wishes â is partially live but contested. The autonomy reading prevents mislabeling by distinguishing the genuine coordination function (respecting self-determination) from the extraction that occurs when procedural gatekeeping denies access or compels professional participation. If the coordination function atrophied entirely into pure gatekeeping, it would degrade toward snare; if the gatekeeping were removed and access universalized, it would approach rope. The current configuration retains both, warranting tangled_rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sibling_reading_foreclosure,
    'Does the autonomy reading''s core premise of sovereign individual authority over death logically foreclose the sanctity reading''s claim of intrinsic inviolable life value, or can they coexist in a single normative framework?',
    'Jurisprudential analysis of hybrid frameworks (e.g., dignity-based approaches that incorporate both autonomy and sanctity) versus pure autonomy regimes.',
    'If foreclosed, the kernel is irreconcilable and political compromise is unstable; if coexistent, hybrid regulatory frameworks are structurally viable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure, conceptual, 'Whether autonomy and sanctity readings are logically mutually exclusive or publicly coexistent').

omega_variable(
    competence_assessment_function,
    'Does competence assessment in assisted dying frameworks function as a genuine protective filter or as performative gatekeeping that denies autonomy to structurally disadvantaged patients?',
    'Comparative analysis of competence assessment outcomes across socioeconomic, racial, and diagnostic categories.',
    'If performative, theater_ratio is higher than measured and suffering_prolonged_patients are systematically generated by the constraint itself rather than by incidental implementation failure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(competence_assessment_function, empirical, 'Whether competence assessments are functional safeguards or bureaucratic theater').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of alternative decision-making frameworks (family veto, physician paternalism, sanctity-based refusal) structural or internalized?',
    'Post-legalization trajectory analysis: if physician and family deference to patient sovereignty persists even after legal repeal, suppression is partially internalized.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests â the medical profession carries the suppression independently of legal enforcement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression mechanism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(end_of_life_decision_authority__autonomy_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(end__tr_t0, end_of_life_decision_authority__autonomy_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(end__tr_t6, end_of_life_decision_authority__autonomy_reading, theater_ratio, 6, 0.24).
narrative_ontology:measurement(end__tr_t12, end_of_life_decision_authority__autonomy_reading, theater_ratio, 12, 0.28).
narrative_ontology:measurement(end__tr_t18, end_of_life_decision_authority__autonomy_reading, theater_ratio, 18, 0.32).
narrative_ontology:measurement(end__tr_t24, end_of_life_decision_authority__autonomy_reading, theater_ratio, 24, 0.36).
narrative_ontology:measurement(end__tr_t30, end_of_life_decision_authority__autonomy_reading, theater_ratio, 30, 0.4).

% Extraction over time
narrative_ontology:measurement(end__be_t0, end_of_life_decision_authority__autonomy_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(end__be_t6, end_of_life_decision_authority__autonomy_reading, base_extractiveness, 6, 0.4).
narrative_ontology:measurement(end__be_t12, end_of_life_decision_authority__autonomy_reading, base_extractiveness, 12, 0.45).
narrative_ontology:measurement(end__be_t18, end_of_life_decision_authority__autonomy_reading, base_extractiveness, 18, 0.5).
narrative_ontology:measurement(end__be_t24, end_of_life_decision_authority__autonomy_reading, base_extractiveness, 24, 0.55).
narrative_ontology:measurement(end__be_t30, end_of_life_decision_authority__autonomy_reading, base_extractiveness, 30, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(end__su_t0, end_of_life_decision_authority__autonomy_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(end__su_t6, end_of_life_decision_authority__autonomy_reading, suppression_requirement, 6, 0.45).
narrative_ontology:measurement(end__su_t12, end_of_life_decision_authority__autonomy_reading, suppression_requirement, 12, 0.48).
narrative_ontology:measurement(end__su_t18, end_of_life_decision_authority__autonomy_reading, suppression_requirement, 18, 0.5).
narrative_ontology:measurement(end__su_t24, end_of_life_decision_authority__autonomy_reading, suppression_requirement, 24, 0.51).
narrative_ontology:measurement(end__su_t30, end_of_life_decision_authority__autonomy_reading, suppression_requirement, 30, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(end_of_life_decision_authority__autonomy_reading, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
