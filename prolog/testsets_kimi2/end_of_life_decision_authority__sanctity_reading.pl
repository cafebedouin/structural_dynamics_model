% ============================================================================
% CONSTRAINT STORY: end_of_life_decision_authority__sanctity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_end_of_life_decision_authority__sanctity_reading, []).

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
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: end_of_life_decision_authority__sanctity_reading
 *   human_readable: Sanctity of Life Prohibition on Intentional Life-Ending
 *   domain: medical ethics/bioethics
 *
 * SUMMARY:
 *   This constraint is the sanctity_reading of the contested kernel
 *   end_of_life_decision_authority. It posits that human life possesses
 *   intrinsic value independent of individual will, making intentional
 *   life-ending an absolute violation. The constraint coordinates medical
 *   practice and social morality around a bright-line prohibition on killing,
 *   while asymmetrically externalizing the cost of that prohibition onto
 *   terminally ill patients who are denied euthanasia. Sibling readings are
 *   autonomy_reading (competent individuals possess sovereign authority over
 *   their own death) and vulnerability_protection_reading (authority must be
 *   distributed across institutional checkpoints to prevent both denial and
 *   coercion).
 *
 * KEY AGENTS:
 *   - medical_profession (institutional/identity_locked): agenda-setter enforcing the healer/non-killer boundary
 *   - vulnerable_patients (powerless/constrained): beneficiary of protective coordination
 *   - terminally_ill_patients (powerless/trapped): payer bearing the externalized suffering
 *   - autonomy_advocates (organized/constrained): excluded voice rendered morally inadmissible
 *   - bioethics_observer (analytical): analytical seat mapping cross-jurisdictional drift
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(end_of_life_decision_authority__sanctity_reading, 0.48).
domain_priors:suppression_score(end_of_life_decision_authority__sanctity_reading, 0.62).
domain_priors:theater_ratio(end_of_life_decision_authority__sanctity_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(end_of_life_decision_authority__sanctity_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(end_of_life_decision_authority__sanctity_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(end_of_life_decision_authority__sanctity_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(end_of_life_decision_authority__sanctity_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(end_of_life_decision_authority__sanctity_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(end_of_life_decision_authority__sanctity_reading, tangled_rope).
narrative_ontology:human_readable(end_of_life_decision_authority__sanctity_reading, "Sanctity of Life Prohibition on Intentional Life-Ending").
narrative_ontology:topic_domain(end_of_life_decision_authority__sanctity_reading, "medical ethics/bioethics").

domain_priors:requires_active_enforcement(end_of_life_decision_authority__sanctity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(end_of_life_decision_authority__sanctity_reading, '49b320f2-8d4e-43c6-b5df-d0437116fe06').
narrative_ontology:cs_kernel_codification('49b320f2-8d4e-43c6-b5df-d0437116fe06', fixed_text).
narrative_ontology:cs_authority_grounding('49b320f2-8d4e-43c6-b5df-d0437116fe06', lineage).
narrative_ontology:cs_interpretation_layer_present('49b320f2-8d4e-43c6-b5df-d0437116fe06').
narrative_ontology:cs_reading_relation('49b320f2-8d4e-43c6-b5df-d0437116fe06', end_of_life_decision_authority__autonomy_reading, forecloses).
narrative_ontology:cs_reading_relation('49b320f2-8d4e-43c6-b5df-d0437116fe06', end_of_life_decision_authority__vulnerability_protection_reading, influences).
narrative_ontology:cs_axiom('49b320f2-8d4e-43c6-b5df-d0437116fe06', foundational, intrinsic_value_independent_of_will).
narrative_ontology:cs_axiom_status(intrinsic_value_independent_of_will, holdable).
narrative_ontology:cs_axiom_grounding('49b320f2-8d4e-43c6-b5df-d0437116fe06', intrinsic_value_independent_of_will, deontological).
narrative_ontology:cs_axiom('49b320f2-8d4e-43c6-b5df-d0437116fe06', foundational, physician_healer_nonkiller_boundary).
narrative_ontology:cs_axiom_status(physician_healer_nonkiller_boundary, holdable).
narrative_ontology:cs_axiom_grounding('49b320f2-8d4e-43c6-b5df-d0437116fe06', physician_healer_nonkiller_boundary, conventional).
narrative_ontology:cs_reference_frame('49b320f2-8d4e-43c6-b5df-d0437116fe06', sanctity_of_life_framework).
narrative_ontology:cs_drift_state('49b320f2-8d4e-43c6-b5df-d0437116fe06', contemporary_bioethics_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('49b320f2-8d4e-43c6-b5df-d0437116fe06', '').
narrative_ontology:cs_kernel_id(end_of_life_decision_authority__sanctity_reading, end_of_life_decision_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(end_of_life_decision_authority__sanctity_reading, vulnerable_patients).
narrative_ontology:constraint_beneficiary(end_of_life_decision_authority__sanctity_reading, medical_profession).
narrative_ontology:constraint_victim(end_of_life_decision_authority__sanctity_reading, terminally_ill_patients).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Retains institutional authority to define the boundary between healing and killing. Enforces the prohibition on intentional life-ending through medical board discipline, licensing, and professional norms. Benefits from a clear identity as healers rather than killers, which simplifies ethical boundaries and maintains public trust.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__sanctity_reading, medical_profession, agenda_setter,
    institutional, generational, identity_locked, national).

% Elderly, disabled, or economically dependent patients who might face pressure to request death if euthanasia were permitted. Under the sanctity framework, they are shielded from institutional pathways that could transform their vulnerability into a duty to die. They do not choose the constraint but receive its protective coordination.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__sanctity_reading, vulnerable_patients, beneficiary,
    powerless, biographical, constrained, national).

% Competent patients experiencing intractable suffering at the end of life. They are denied access to euthanasia or assisted suicide by the sanctity constraint. Their suffering is borne individually; the social and medical system externalizes the cost of maintaining the absolute prohibition onto their continued existence.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__sanctity_reading, terminally_ill_patients, payer,
    powerless, immediate, trapped, national).

% Right-to-die organizations, some palliative care physicians, and civil liberties groups who argue that sovereign authority over death is a fundamental right. They are excluded from the sanctity framework's moral discourse; their arguments are ruled inadmissible by the premise that intrinsic value transcends individual will.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__sanctity_reading, autonomy_advocates, excluded,
    organized, biographical, constrained, national).

% Academic and policy analysts who track the divergence between sanctity-based prohibition and autonomy-based legalization across jurisdictions. They neither benefit from nor pay the constraint's costs directly; they map its structural effects.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__sanctity_reading, bioethics_observer, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(end_of_life_decision_authority__sanctity_reading, diffuse).
narrative_ontology:fixing_cost_class(end_of_life_decision_authority__sanctity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a bright-line prohibition on intentional killing in medical practice, coordinating physicians, patients, and society around an absolute boundary that prevents the normalization of medical life-ending and protects vulnerable patients from coercion.
% TRANSFER_FUNCTION: Transfers the full burden of intractable end-of-life suffering onto the individual terminally ill patient, while transferring moral clarity and professional identity coherence to the medical profession and society.
% ABSENT_VOICES: Competent terminally ill patients who actively seek euthanasia and autonomy advocates are structurally excluded; their claims are rendered morally illegitimate by the sanctity premise, and they are absent from the forums where the constraint is maintained.
% DISAPPEARANCE_RATIONALE: If the sanctity constraint vanished, medical practice would reorganize to include intentional life-ending; the physician's role would lose its absolute healer/killer boundary; vulnerable patient protections would shift from absolute prohibition to procedural and institutional safeguards; and the social moral order would pivot toward autonomy or vulnerability-management frameworks.
% FOUNDING_PROBLEM: How to prevent medical killing of vulnerable patients and preserve the integrity of the healing profession when faced with suffering and requests for death.
% FOUNDING_PROBLEM_CORROBORATION: Traditional medical ethicists and disability rights advocates within the sanctity tradition attest the founding problem remains live. External corroboration from jurisdictions that have legalized euthanasia (Netherlands, Canada) and from autonomy-oriented bioethicists challenges the severity of the problem, arguing that institutional safeguards can prevent abuse without absolute prohibition; empirical data on vulnerability under legal regimes is cited by both sides.
narrative_ontology:disappearance_verdict(end_of_life_decision_authority__sanctity_reading, world_rearranges).
narrative_ontology:founding_problem_status(end_of_life_decision_authority__sanctity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(end_of_life_decision_authority__sanctity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(end_of_life_decision_authority__sanctity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(end_of_life_decision_authority__sanctity_reading, 0.48, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(end_of_life_decision_authority__sanctity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(end_of_life_decision_authority__sanctity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(end_of_life_decision_authority__sanctity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.48 because the constraint imposes real, non-monetary extraction (prolonged suffering, denied autonomy) on terminally ill patients, but this is partially offset by a genuine coordination function (protecting vulnerable populations from coercion). Suppression at 0.62 reflects active legal and professional prohibition of euthanasia alternatives, plus the identity-locked exit of the medical profession. Theater ratio at 0.30 captures the ritual maintenance of the Hippocratic stance, which remains partly functional rather than purely performative. Accessibility collapse is high (0.75) because once the sanctity premise is accepted, euthanasia becomes morally unthinkable. Resistance at 0.58 reflects sustained right-to-die advocacy and jurisdictional defections. The measurement series share a single time grid so temporal analysis samples every metric at every observed point.
 *
 * PERSPECTIVAL GAP:
 *   From the medical profession's seat and the vulnerable-patient seat, the constraint computes as protective coordination â a necessary guardrail preventing the normalization of killing. From the terminally ill patient seat, the same structure computes as extraction: their suffering is structurally required to maintain the moral boundary. The engine derives this divergence from the identical structural data via directionality; the authored claim does not adjudicate the dispute.
 *
 * DIRECTIONALITY LOGIC:
 *   The medical profession and vulnerable patients are declared beneficiaries, placing them at the low-d (subsidy) end of the directionality spectrum. The terminally ill patients are declared victims (payer role), placing them at the high-d (target) end. The medical profession's identity_locked exit amplifies its structural alignment with the constraint. The extraction is non-monetary: it takes the form of externalized suffering and foregone autonomy rather than a financial transfer, which is why gain_flow is authored as diffuse.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â protecting vulnerable patients from medical killing â remains contested rather than dead, so mandatrophy_resolved is not declared. In jurisdictions where euthanasia has been legalized despite the sanctity framework, the constraint may exhibit piton-like theatrical maintenance (professional rhetoric continuing to assert the healer boundary while practice drifts), but the authored metrics describe the constraint where it remains actively governing.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    suffering_quantification_ambiguity,
    'Can the suffering of terminally ill patients be quantified and morally weighed against the protective coordination the sanctity framework provides to vulnerable populations?',
    'Comparative quality-of-life and patient-preference studies across jurisdictions with and without sanctity-based prohibition, combined with longitudinal data on vulnerable-group outcomes under alternative regimes.',
    'If suffering is irreducibly subjective and severe, the extraction is higher than modeled; if vulnerable-group protection collapses without absolute prohibition, the coordination function justifies the extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suffering_quantification_ambiguity, conceptual, 'Whether individual suffering can be weighed against collective protection').

omega_variable(
    abuse_forecast_accuracy,
    'Does the sanctity framework''s prediction of widespread abuse of vulnerable patients under legalized euthanasia hold empirically?',
    'Empirical analysis of vulnerability-indicator trends (socioeconomic, disability, elderly) in jurisdictions with legal euthanasia versus prohibition jurisdictions over equivalent time horizons.',
    'If abuse is rare under legalized regimes with safeguards, the coordination benefit of absolute prohibition is lower than claimed, strengthening the extraction reading; if abuse is widespread, the coordination benefit is validated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(abuse_forecast_accuracy, empirical, 'Empirical accuracy of the abuse prediction underlying sanctity coordination').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of euthanasia alternatives under the sanctity framework primarily structural (legal and professional prohibition) or internalized (professional identity fusion, patient moral beliefs)?',
    'Post-legalization trajectory analysis in jurisdictions that removed prohibition: if euthanasia uptake remains low despite legality, suppression was partially internalized; if uptake surges, suppression was primarily structural.',
    'If internalized, effective suppression exceeds the structural measure and the constraint operates more deeply; if structural only, removal of legal barriers would rapidly reclassify the constraint.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized suppression mechanism in sanctity constraint').

omega_variable(
    sanctity_vulnerability_framing,
    'Does the sanctity reading foreclose the vulnerability_protection reading, or does it merely influence its operating conditions?',
    'Analysis of whether a coherent single framework can hold both absolute prohibition on intentional killing AND distributed institutional checkpoints designed to prevent denial of requested death.',
    'If foreclosed, the kernel contains logically irreconcilable readings; if influential only, intermediate regulatory architectures remain structurally viable.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sanctity_vulnerability_framing, conceptual, 'Logical relationship between sanctity and vulnerability-protection readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(end_of_life_decision_authority__sanctity_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eol_sanctity_tr_t0, end_of_life_decision_authority__sanctity_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(eol_sanctity_tr_t10, end_of_life_decision_authority__sanctity_reading, theater_ratio, 10, 0.2).
narrative_ontology:measurement(eol_sanctity_tr_t20, end_of_life_decision_authority__sanctity_reading, theater_ratio, 20, 0.24).
narrative_ontology:measurement(eol_sanctity_tr_t30, end_of_life_decision_authority__sanctity_reading, theater_ratio, 30, 0.27).
narrative_ontology:measurement(eol_sanctity_tr_t40, end_of_life_decision_authority__sanctity_reading, theater_ratio, 40, 0.3).

% Extraction over time
narrative_ontology:measurement(eol_sanctity_be_t0, end_of_life_decision_authority__sanctity_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(eol_sanctity_be_t10, end_of_life_decision_authority__sanctity_reading, base_extractiveness, 10, 0.38).
narrative_ontology:measurement(eol_sanctity_be_t20, end_of_life_decision_authority__sanctity_reading, base_extractiveness, 20, 0.42).
narrative_ontology:measurement(eol_sanctity_be_t30, end_of_life_decision_authority__sanctity_reading, base_extractiveness, 30, 0.45).
narrative_ontology:measurement(eol_sanctity_be_t40, end_of_life_decision_authority__sanctity_reading, base_extractiveness, 40, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(eol_sanctity_su_t0, end_of_life_decision_authority__sanctity_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(eol_sanctity_su_t10, end_of_life_decision_authority__sanctity_reading, suppression_requirement, 10, 0.5).
narrative_ontology:measurement(eol_sanctity_su_t20, end_of_life_decision_authority__sanctity_reading, suppression_requirement, 20, 0.55).
narrative_ontology:measurement(eol_sanctity_su_t30, end_of_life_decision_authority__sanctity_reading, suppression_requirement, 30, 0.6).
narrative_ontology:measurement(eol_sanctity_su_t40, end_of_life_decision_authority__sanctity_reading, suppression_requirement, 40, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(end_of_life_decision_authority__sanctity_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
