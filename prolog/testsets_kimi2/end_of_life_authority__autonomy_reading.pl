% ============================================================================
% CONSTRAINT STORY: end_of_life_authority__autonomy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_end_of_life_authority__autonomy_reading, []).

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
 *   constraint_id: end_of_life_authority__autonomy_reading
 *   human_readable: Autonomy-Based Medical Assistance in Dying Framework
 *   domain: medical ethics/bioethics/end-of-life policy
 *
 * SUMMARY:
 *   This constraint story instantiates the autonomy reading of the
 *   end_of_life_authority kernel: the standing legal-medical arrangement that
 *   grounds end-of-life decision-making in individual patient autonomy rather
 *   than paternalistic or sanctity-based authority. The arrangement
 *   coordinates a regulated pathway for assisted dying while extracting from
 *   patients who fall outside eligibility criteria and from clinicians
 *   compelled to participate against conscience. The claim is
 *   tangled_ropeâgenuine coordination (solving the problem of unregulated,
 *   violent suicide and futile prolongation) combined with asymmetric
 *   extraction (residual paternalistic gatekeeping that prolongs suffering
 *   for the excluded and compels objecting providers). The metrics are
 *   authored independently of the claim: suppression is high because the
 *   constraint must actively override prohibitionist and paternalistic
 *   alternatives; extractiveness is moderate-high because the framework still
 *   denies autonomy to a substantial excluded population.
 *
 * KEY AGENTS:
 *   - Eligible patients: Primary beneficiary (moderate/constrained)âgain a legal pathway but must navigate gatekeeping.
 *   - Excluded patients: Primary target (powerless/trapped)âdenied access and forced to endure suffering or seek dangerous alternatives.
 *   - Objecting clinicians: Secondary target (organized/constrained)âcompelled to refer or participate against moral convictions.
 *   - Medical regulators: Agenda setter (institutional/arbitrage)âdesign criteria, expand scope over time, enforce against prohibition.
 *   - Sanctity advocates: Excluded seat (organized/constrained)âstructurally silenced by the framework's suppression of paternalistic alternatives.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(end_of_life_authority__autonomy_reading, 0.62).
domain_priors:suppression_score(end_of_life_authority__autonomy_reading, 0.82).
domain_priors:theater_ratio(end_of_life_authority__autonomy_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(end_of_life_authority__autonomy_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(end_of_life_authority__autonomy_reading, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(end_of_life_authority__autonomy_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(end_of_life_authority__autonomy_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(end_of_life_authority__autonomy_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(end_of_life_authority__autonomy_reading, tangled_rope).
narrative_ontology:human_readable(end_of_life_authority__autonomy_reading, "Autonomy-Based Medical Assistance in Dying Framework").
narrative_ontology:topic_domain(end_of_life_authority__autonomy_reading, "medical ethics/bioethics/end-of-life policy").

domain_priors:requires_active_enforcement(end_of_life_authority__autonomy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(end_of_life_authority__autonomy_reading, '8edf16d7-93a2-4f38-b123-2ea53d549bbe').
narrative_ontology:cs_kernel_codification('8edf16d7-93a2-4f38-b123-2ea53d549bbe', formalized).
narrative_ontology:cs_authority_grounding('8edf16d7-93a2-4f38-b123-2ea53d549bbe', expertise).
narrative_ontology:cs_interpretation_layer_present('8edf16d7-93a2-4f38-b123-2ea53d549bbe').
narrative_ontology:cs_reading_relation('8edf16d7-93a2-4f38-b123-2ea53d549bbe', end_of_life_authority__sanctity_reading, forecloses).
narrative_ontology:cs_reading_relation('8edf16d7-93a2-4f38-b123-2ea53d549bbe', end_of_life_authority__slippery_slope_mechanism, influences).
narrative_ontology:cs_axiom('8edf16d7-93a2-4f38-b123-2ea53d549bbe', foundational, individual_autonomy_as_ultimate_authority_over_mortality).
narrative_ontology:cs_axiom_status(individual_autonomy_as_ultimate_authority_over_mortality, holdable).
narrative_ontology:cs_axiom_grounding('8edf16d7-93a2-4f38-b123-2ea53d549bbe', individual_autonomy_as_ultimate_authority_over_mortality, deontological).
narrative_ontology:cs_axiom('8edf16d7-93a2-4f38-b123-2ea53d549bbe', foundational, state_duty_to_facilitate_exit_from_irremediable_suffering).
narrative_ontology:cs_axiom_status(state_duty_to_facilitate_exit_from_irremediable_suffering, holdable).
narrative_ontology:cs_axiom_grounding('8edf16d7-93a2-4f38-b123-2ea53d549bbe', state_duty_to_facilitate_exit_from_irremediable_suffering, deontological).
narrative_ontology:cs_reference_frame('8edf16d7-93a2-4f38-b123-2ea53d549bbe', competent_terminal_patient_sovereignty).
narrative_ontology:cs_drift_state('8edf16d7-93a2-4f38-b123-2ea53d549bbe', contemporary_expansion_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('8edf16d7-93a2-4f38-b123-2ea53d549bbe', '').
narrative_ontology:cs_kernel_id(end_of_life_authority__autonomy_reading, end_of_life_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(end_of_life_authority__autonomy_reading, eligible_patients).
narrative_ontology:constraint_victim(end_of_life_authority__autonomy_reading, excluded_patients).
narrative_ontology:constraint_victim(end_of_life_authority__autonomy_reading, objecting_clinicians).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Competent adults who meet statutory eligibility criteria for medical assistance in dying, gaining a legally supervised pathway to control the timing and manner of death. They avoid unregulated methods but must navigate mandatory waiting periods, multiple independent assessments, and scarcity of willing providers.
narrative_ontology:constraint_stakeholder(end_of_life_authority__autonomy_reading, eligible_patients, beneficiary,
    moderate, immediate, constrained, national).

% Patients with irremediable suffering that falls outside current eligibility boundariesâsuch as those whose sole underlying condition is mental illness, those not yet terminal, or those with fluctuating competenceâwho are denied legal access to assisted dying and forced to endure prolonged suffering or pursue dangerous extralegal alternatives.
narrative_ontology:constraint_stakeholder(end_of_life_authority__autonomy_reading, excluded_patients, payer,
    powerless, immediate, trapped, national).

% Physicians, nurses, and faith-based institutions with moral or religious objections to assisted dying who are compelled by law or professional standard to provide effective referral or to participate in assessment processes against their conscience, subordinating their professional autonomy to the patient-autonomy framework.
narrative_ontology:constraint_stakeholder(end_of_life_authority__autonomy_reading, objecting_clinicians, payer,
    organized, biographical, constrained, national).

% Medical regulatory colleges, health ministries, and legislative bodies that draft eligibility criteria, standards of practice, and reporting requirements. They adjudicate boundary cases, expand or restrict criteria over time, and actively enforce the framework against paternalistic or prohibitionist alternatives.
narrative_ontology:constraint_stakeholder(end_of_life_authority__autonomy_reading, medical_regulators, agenda_setter,
    institutional, generational, arbitrage, national).

% Religious institutions, disability-rights organizations, and medical paternalists who oppose assisted dying on sanctity-of-life or vulnerability grounds. They are structurally excluded from individual patient decisions and their preferred constraintâblanket prohibitionâis actively suppressed by the autonomy framework.
narrative_ontology:constraint_stakeholder(end_of_life_authority__autonomy_reading, sanctity_advocates, excluded,
    organized, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Replaces unregulated, dangerous, or criminalized suicide with a structured, medically supervised legal pathway for competent adults facing unbearable suffering, establishing clear procedures, liability shields, and documentation standards.
% TRANSFER_FUNCTION: Moves decisional authority over end-of-life timing from paternalistic medical and legal institutions to the individual patient, while transferring procedural burden, legal liability, and professional compliance costs to physicians and the state.
% ABSENT_VOICES: Patients with cognitive impairments that preclude competence assessments, mature minors seeking end-of-life autonomy, and collectivist or sanctity-based cultural traditions are structurally excluded from the policy conversation; their absence is treated as legitimate boundary-drawing rather than a gap.
% DISAPPEARANCE_RATIONALE: If the autonomy framework vanished overnight, eligible patients would lose a legal pathway and revert to violent or clandestine methods; physicians would face criminal ambiguity; and decisional authority over end-of-life would recentralize to paternalistic or prohibitionist institutions. The world rearranges because multiple institutional arrangements depend on the constraint's existence.
% FOUNDING_PROBLEM: Unbearable suffering at end of life was met either with futile medical prolongation or with unassisted, often violent suicide; patients lacked legal standing to request and receive assistance in dying, and physicians who provided such assistance faced criminal prosecution.
% FOUNDING_PROBLEM_CORROBORATION: Independent palliative-care researchers and patient advocacy organizations outside the state apparatus attest that some suffering is irremediable by palliation. Religious, disability-rights, and sanctity-based advocates contest that the founding problem justifies the current arrangement, arguing that improved palliative care and social support eliminate the need. Legislative hearing testimony and peer-reviewed outcome studies from non-beneficiary seats corroborate the persistence of irremediable suffering; theological critics deny it.
narrative_ontology:disappearance_verdict(end_of_life_authority__autonomy_reading, world_rearranges).
narrative_ontology:founding_problem_status(end_of_life_authority__autonomy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(end_of_life_authority__autonomy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(end_of_life_authority__autonomy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(end_of_life_authority__autonomy_reading, 0.62, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(end_of_life_authority__autonomy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(end_of_life_authority__autonomy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(end_of_life_authority__autonomy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) reflects that the framework still extracts heavily from the excluded population by denying them relief and from objecting clinicians by compelling participation. Suppression (0.82) is high because the constraint's persistence depends on actively suppressing the prohibitionist alternative that would otherwise dominate. Theater ratio (0.42) captures that procedural requirements include both genuine safety coordination and performative delay. Accessibility collapse (0.58) is moderate: the legal pathway is well-defined, but extralegal alternatives remain dangerous and stigmatized rather than fully collapsed. Resistance (0.52) reflects ongoing legislative battles, conscientious objection, and litigation. Temporal measurements show extraction declining slightly as eligibility expands, theater rising with added procedural complexity, and suppression requirement increasing as expansion meets intensifying opposition.
 *
 * PERSPECTIVAL GAP:
 *   The eligible patient seat and the excluded patient seat compute as different types despite facing the same nominal constraint, because eligibility criteria differentiate their exit options and directionalities. The regulator seat computes as coordination administrator, while the objecting clinician seat computes as extraction target. The engine captures this divergence from the structural data without requiring reconciled classification.
 *
 * DIRECTIONALITY LOGIC:
 *   Eligible patients are structural beneficiaries (d near beneficiary end): they receive the coordination good of a safe legal pathway. Excluded patients are full targets (d near 1.0): the constraint extracts from them by denying the same autonomy, and their exit is trapped by eligibility walls. Objecting clinicians are targets (d high): the constraint extracts professional autonomy from them through compelled referral. Medical regulators sit near the beneficiary end with arbitrage-grade exitâthey could modify the constraint. Sanctity advocates are excluded with constrained exit: their preferred framework is suppressed, making them structural targets of the constraint's enforcement machinery.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification prevents mislabeling the autonomy framework as pure coordination (rope) by insisting on naming the excluded patient population as victims of residual paternalistic gatekeeping. It also prevents mislabeling it as pure extraction (snare) by acknowledging the genuine coordination function for eligible patients and the real problem of unregulated suffering it solves. The R5 genealogy corroboration requirement surfaces the contested status of the founding problem, preventing the autonomy reading from simply authorizing itself.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    autonomy_reading_sibling_divergence,
    'How would the structural classification change if the sanctity reading or slippery-slope reading of the same kernel were adopted instead of the autonomy reading?',
    'Cross-reading comparison: the sanctity reading would reclassify the same arrangement as a snare targeting the intrinsic value of human life; the slippery-slope reading would project rising extractiveness as eligibility expands to vulnerable populations.',
    'The autonomy reading treats current extraction as residual denial of patient rights that declines as criteria expand; sibling readings would treat the same expansion as either categorical moral evil or empirical evidence of capture.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(autonomy_reading_sibling_divergence, conceptual, 'Kernel reading contest for end-of-life authority').

omega_variable(
    expansion_authenticity,
    'Does the empirical expansion of eligibility criteria reflect the genuine extension of autonomy to previously excluded sufferers, or does it constitute creeping extraction via loosened safeguards that captures vulnerable populations?',
    'Longitudinal outcome data comparing pre-expansion and post-expansion MAID cohorts for vulnerability markers such as socioeconomic pressure, isolation, and non-terminal status.',
    'If expansion tracks vulnerability rather than irremediable suffering, the autonomy framework''s extractiveness is higher than the reading assumes and the slippery-slope reading gains empirical support.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(expansion_authenticity, empirical, 'Whether expanding criteria serve autonomy or capture vulnerable populations').

omega_variable(
    gatekeeping_as_coordination_or_extraction,
    'Does mandatory medical gatekeepingâmultiple assessors, waiting periods, and competence panelsâserve genuine coordination (preventing error and ensuring voluntariness) or extractive theater (delaying access while maintaining professional control over mortality decisions)?',
    'Comparative analysis of jurisdictions with minimal versus extensive procedural gatekeeping, measuring error rates, patient satisfaction, and incidence of coercion or regret.',
    'If minimal gatekeeping produces equivalent safety outcomes, the extensive procedural requirements are extractive theater and the constraint shifts toward snare-like operation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gatekeeping_as_coordination_or_extraction, conceptual, 'Whether procedural requirements are coordination cost or extraction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(end_of_life_authority__autonomy_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eol_autonomy_tr_t0, end_of_life_authority__autonomy_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(eol_autonomy_tr_t5, end_of_life_authority__autonomy_reading, theater_ratio, 5, 0.3).
narrative_ontology:measurement(eol_autonomy_tr_t10, end_of_life_authority__autonomy_reading, theater_ratio, 10, 0.35).
narrative_ontology:measurement(eol_autonomy_tr_t15, end_of_life_authority__autonomy_reading, theater_ratio, 15, 0.4).
narrative_ontology:measurement(eol_autonomy_tr_t20, end_of_life_authority__autonomy_reading, theater_ratio, 20, 0.44).
narrative_ontology:measurement(eol_autonomy_tr_t25, end_of_life_authority__autonomy_reading, theater_ratio, 25, 0.48).

% Extraction over time
narrative_ontology:measurement(eol_autonomy_be_t0, end_of_life_authority__autonomy_reading, base_extractiveness, 0, 0.72).
narrative_ontology:measurement(eol_autonomy_be_t5, end_of_life_authority__autonomy_reading, base_extractiveness, 5, 0.68).
narrative_ontology:measurement(eol_autonomy_be_t10, end_of_life_authority__autonomy_reading, base_extractiveness, 10, 0.63).
narrative_ontology:measurement(eol_autonomy_be_t15, end_of_life_authority__autonomy_reading, base_extractiveness, 15, 0.58).
narrative_ontology:measurement(eol_autonomy_be_t20, end_of_life_authority__autonomy_reading, base_extractiveness, 20, 0.53).
narrative_ontology:measurement(eol_autonomy_be_t25, end_of_life_authority__autonomy_reading, base_extractiveness, 25, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(eol_autonomy_su_t0, end_of_life_authority__autonomy_reading, suppression_requirement, 0, 0.75).
narrative_ontology:measurement(eol_autonomy_su_t5, end_of_life_authority__autonomy_reading, suppression_requirement, 5, 0.78).
narrative_ontology:measurement(eol_autonomy_su_t10, end_of_life_authority__autonomy_reading, suppression_requirement, 10, 0.8).
narrative_ontology:measurement(eol_autonomy_su_t15, end_of_life_authority__autonomy_reading, suppression_requirement, 15, 0.82).
narrative_ontology:measurement(eol_autonomy_su_t20, end_of_life_authority__autonomy_reading, suppression_requirement, 20, 0.84).
narrative_ontology:measurement(eol_autonomy_su_t25, end_of_life_authority__autonomy_reading, suppression_requirement, 25, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(end_of_life_authority__autonomy_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(end_of_life_authority__autonomy_reading, sanctity_reading).
narrative_ontology:affects_constraint(end_of_life_authority__autonomy_reading, slippery_slope_mechanism).

% DUAL FORMULATION NOTE:
% The end_of_life_authority kernel decomposes into three structurally distinct constraints: the autonomy reading (this file), which treats patient choice as sovereign; the sanctity reading, which treats intentional life-ending as categorically prohibited; and the slippery_slope_mechanism reading, which tracks empirical expansion beyond original boundaries. Each reading carries a different Îµ, beneficiary/victim structure, and classification. They are linked as a constraint family because they compete to define the same institutional domain.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
