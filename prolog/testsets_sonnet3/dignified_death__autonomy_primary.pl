% ============================================================================
% CONSTRAINT STORY: dignified_death__autonomy_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dignified_death__autonomy_primary, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: dignified_death__autonomy_primary
 *   human_readable: Autonomy-Primary Reading of the Right to Die: Physician-Mediated Access Regime
 *   domain: bioethics/medical_law/political_philosophy
 *
 * SUMMARY:
 *   This story instantiates the autonomy_primary reading of the
 *   dignified-death kernel: dignity is located in the suffering individual's
 *   self-determination, and the individual holds final authority over the
 *   timing and method of their death. Under this reading, state prohibition
 *   of assisted death is a high-extraction constraint (ε 0.52-0.58 across the
 *   interval) because it denies exit to people suffering against their will —
 *   the paradigmatic victim class. Where jurisdictions have legalized
 *   assisted death under this reading, the resulting regime is not a pure
 *   rope: the autonomy norm becomes entangled with medical gatekeeping
 *   (terminal-diagnosis requirements, waiting periods, multi-physician
 *   sign-off) that both legitimizes the arrangement politically and produces
 *   a new excluded class — chronically suffering non-terminal patients whose
 *   self-determination claim is, by this reading's own logic, equally valid
 *   but institutionally unrecognized. That is the tangled_rope signature:
 *   genuine coordination function (a legally administrable, auditable path to
 *   exercising the autonomy right) fused with asymmetric extraction
 *   (institutions capture procedural authority and legitimacy; excluded
 *   patients bear continued suffering).
 *
 * KEY AGENTS:
 *   - suffering_patients_denied_or_delayed_by_eligibility_criteria: primary target within regulated jurisdictions — bears extraction through delay/denial
 *   - non_terminal_chronic_suffering_patients_excluded_by_diagnosis_rules: excluded target — the autonomy claim's own logic would include them but eligibility rules exclude
 *   - patients_in_jurisdictions_with_full_prohibition: primary target under total prohibition — the clearest case this reading identifies
 *   - eligible_terminal_patients_who_qualify: primary beneficiary — obtains the sanctioned exit
 *   - medical_boards_administering_eligibility_review / state_licensing_authorities: institutional agenda-setters/beneficiaries — administer and legitimize the gatekeeping apparatus
 *   - disability_rights_advocates: excluded analytical voice — raises the systemic-pressure objection largely absent from autonomy-primary debate
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dignified_death__autonomy_primary, 0.52).
domain_priors:suppression_score(dignified_death__autonomy_primary, 0.68).
domain_priors:theater_ratio(dignified_death__autonomy_primary, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dignified_death__autonomy_primary, extractiveness, 0.52).
narrative_ontology:constraint_metric(dignified_death__autonomy_primary, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(dignified_death__autonomy_primary, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dignified_death__autonomy_primary, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(dignified_death__autonomy_primary, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dignified_death__autonomy_primary, tangled_rope).
narrative_ontology:human_readable(dignified_death__autonomy_primary, "Autonomy-Primary Reading of the Right to Die: Physician-Mediated Access Regime").
narrative_ontology:topic_domain(dignified_death__autonomy_primary, "bioethics/medical_law/political_philosophy").

domain_priors:requires_active_enforcement(dignified_death__autonomy_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dignified_death__autonomy_primary, 'b6c870ce-297f-41eb-a2f4-7807e780b7d3').
narrative_ontology:cs_kernel_codification('b6c870ce-297f-41eb-a2f4-7807e780b7d3', distributed).
narrative_ontology:cs_authority_grounding('b6c870ce-297f-41eb-a2f4-7807e780b7d3', distributed).
narrative_ontology:cs_reading_relation('b6c870ce-297f-41eb-a2f4-7807e780b7d3', dignified_death__sanctity_primary, forecloses).
narrative_ontology:cs_reading_relation('b6c870ce-297f-41eb-a2f4-7807e780b7d3', dignified_death__relational_autonomy, influences).
narrative_ontology:cs_axiom('b6c870ce-297f-41eb-a2f4-7807e780b7d3', foundational, individual_self_determination_grounds_dignity).
narrative_ontology:cs_axiom_status(individual_self_determination_grounds_dignity, holdable).
narrative_ontology:cs_axiom_grounding('b6c870ce-297f-41eb-a2f4-7807e780b7d3', individual_self_determination_grounds_dignity, deontological).
narrative_ontology:cs_axiom('b6c870ce-297f-41eb-a2f4-7807e780b7d3', foundational, suffering_individual_holds_final_authority_over_death_timing).
narrative_ontology:cs_axiom_status(suffering_individual_holds_final_authority_over_death_timing, holdable).
narrative_ontology:cs_axiom_grounding('b6c870ce-297f-41eb-a2f4-7807e780b7d3', suffering_individual_holds_final_authority_over_death_timing, deontological).
narrative_ontology:cs_reference_frame('b6c870ce-297f-41eb-a2f4-7807e780b7d3', common_law_prohibition_of_assisted_death).
narrative_ontology:cs_drift_state('b6c870ce-297f-41eb-a2f4-7807e780b7d3', post_legalization_wave_contemporary, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('b6c870ce-297f-41eb-a2f4-7807e780b7d3', '').
narrative_ontology:cs_kernel_id(dignified_death__autonomy_primary, dignified_death).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dignified_death__autonomy_primary, eligible_terminal_patients_who_qualify).
narrative_ontology:constraint_beneficiary(dignified_death__autonomy_primary, medical_boards_administering_eligibility_review).
narrative_ontology:constraint_beneficiary(dignified_death__autonomy_primary, state_licensing_authorities).
narrative_ontology:constraint_victim(dignified_death__autonomy_primary, suffering_patients_denied_or_delayed_by_eligibility_criteria).
narrative_ontology:constraint_victim(dignified_death__autonomy_primary, non_terminal_chronic_suffering_patients_excluded_by_diagnosis_rules).
narrative_ontology:constraint_victim(dignified_death__autonomy_primary, patients_in_jurisdictions_with_full_prohibition).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(dignified_death__autonomy_primary, attending_physicians_and_hospice_clinicians).
narrative_ontology:constraint_vindicates(dignified_death__autonomy_primary, self_determination_as_locus_of_dignity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Experience unrelenting suffering and want the timing and method of death under their own control, but face waiting periods, capacity assessments, prognosis requirements (often a 6-month terminal window), and multi-physician sign-off. Some die during the waiting period before approval completes; some are found ineligible because their suffering is not classified as terminal. Their exit from suffering is mediated entirely by institutions they do not control.
narrative_ontology:constraint_stakeholder(dignified_death__autonomy_primary, suffering_patients_denied_or_delayed_by_eligibility_criteria, payer,
    powerless, immediate, trapped, national).

% Suffer from degenerative, chronic, or psychiatric conditions that are not terminal by statutory definition. Under the autonomy-primary reading their self-determination claim is structurally identical to a terminal patient's, but the medical gatekeeping apparatus built to legitimize the regime for legislators and courts excludes them categorically. They have no legal path to a supported death regardless of how they weigh their own suffering.
narrative_ontology:constraint_stakeholder(dignified_death__autonomy_primary, non_terminal_chronic_suffering_patients_excluded_by_diagnosis_rules, payer,
    powerless, biographical, trapped, national).

% Meet the statutory terminal-diagnosis and capacity criteria and obtain access to a legally sanctioned death on their own timeline. They experience the arrangement as coordination working exactly as intended — dignity restored through control over the end of life — though even they must navigate waiting periods and multiple physician evaluations before the authority is exercised.
narrative_ontology:constraint_stakeholder(dignified_death__autonomy_primary, eligible_terminal_patients_who_qualify, beneficiary,
    powerless, immediate, constrained, national).

% Design and enforce the eligibility criteria, capacity assessments, and waiting-period protocols that operationalize the autonomy claim into a legally administrable procedure. Their institutional legitimacy and continued authority depend on maintaining a gatekeeping apparatus around the autonomy right; without eligibility review the autonomy claim would need no institution to certify it.
narrative_ontology:constraint_stakeholder(dignified_death__autonomy_primary, medical_boards_administering_eligibility_review, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(dignified_death__autonomy_primary, medical_boards_administering_eligibility_review, beneficiary).

% Grant legal cover for physicians participating in assisted death and calibrate the statutory boundaries (who counts as terminal, how many waiting days, how many physician signatures). They benefit from being able to present the regime to the public and courts as safely bounded, which sustains its political survival, but the bounding is exactly what produces the excluded classes.
narrative_ontology:constraint_stakeholder(dignified_death__autonomy_primary, state_licensing_authorities, beneficiary,
    institutional, civilizational, analytical, national).
narrative_ontology:stakeholder_secondary_role(dignified_death__autonomy_primary, state_licensing_authorities, agenda_setter).

% Live under a total ban and have no legal exit at all; the autonomy-primary reading treats their situation as the clearest case of denied dignity — indefinite suffering imposed by a state that supplies no timing-and-method authority whatsoever. Some travel abroad if wealthy and mobile enough; most cannot.
narrative_ontology:constraint_stakeholder(dignified_death__autonomy_primary, patients_in_jurisdictions_with_full_prohibition, payer,
    powerless, immediate, trapped, national).

% Argue that framing dignity purely in terms of individual self-determination, absent attention to systemic undertreatment of disability and inadequate palliative and social support, creates pressure that disproportionately channels disabled and impoverished patients toward assisted death rather than toward resourcing their continued living. Their objection is largely absent from autonomy-primary legislative debates, which center the suffering individual's choice rather than the conditions producing the choice.
narrative_ontology:constraint_stakeholder(dignified_death__autonomy_primary, disability_rights_advocates, excluded,
    organized, generational, constrained, national).

% Bear direct legal and professional risk when certifying eligibility and participating in the procedure; conscientious objectors must navigate referral obligations. They administer the gatekeeping the autonomy claim depends on, at personal legal and psychological cost, without receiving the dignity benefit themselves.
narrative_ontology:constraint_stakeholder(dignified_death__autonomy_primary, attending_physicians_and_hospice_clinicians, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(dignified_death__autonomy_primary, attending_physicians_and_hospice_clinicians, agenda_setter).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(dignified_death__autonomy_primary, medical_boards_administering_eligibility_review).
narrative_ontology:fixing_cost_class(dignified_death__autonomy_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a legally administrable path by which a state can recognize an individual's authority over the timing and method of their own death while giving courts, legislators, and the public an auditable, bounded procedure they can certify as safe — solving the genuine problem that unregulated self-administered death carries risk of coercion, error, and irreversibility.
% TRANSFER_FUNCTION: Moves the authority to end suffering from the state (which previously held a monopoly on prohibiting it) toward the individual, but only through a channel administered by medical and legal institutions — those institutions capture procedural authority and legitimacy in the transfer, while patients outside the eligibility boundary receive nothing.
% ABSENT_VOICES: Disability rights advocates who worry the individual-choice framing masks systemic undertreatment are largely outside the autonomy-primary legislative conversation. Non-terminal chronically suffering patients who would claim the same self-determination right are excluded from the room by the terminal-diagnosis eligibility line itself.
% DISAPPEARANCE_RATIONALE: If the autonomy-primary right and its administering apparatus vanished, patients currently exercising a legally sanctioned death on their own timeline would revert to either prohibition, covert self-administered methods, or prolonged suffering; physicians would lose legal cover for participation; medical boards and licensing authorities would lose an entire domain of regulatory jurisdiction. The arrangement is load-bearing for a specific population's actual end-of-life decisions, not a redundant label on a state of affairs that would persist regardless.
% FOUNDING_PROBLEM: Terminally ill and irreversibly suffering individuals were being kept alive against their expressed will by a legal regime that treated all intentional hastening of death as homicide or assisted suicide, regardless of consent, producing prolonged suffering with no legal exit.
% FOUNDING_PROBLEM_CORROBORATION: Courts (e.g. in jurisdictions recognizing a constitutional liberty interest) and legislative committees drawing on patient testimony corroborate that the founding problem was real and remains live for the eligible population. Disability rights organizations and some palliative care researchers — outside the beneficiary set of patients and administering institutions — corroborate a different, contested status: that the problem has been only partially solved and that the eligibility architecture has created a new, narrower problem (exclusion of non-terminal suffering) rather than fully resolving the original one.
narrative_ontology:disappearance_verdict(dignified_death__autonomy_primary, world_rearranges).
narrative_ontology:founding_problem_status(dignified_death__autonomy_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dignified_death__autonomy_primary, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(dignified_death__autonomy_primary, 'none', 1).
narrative_ontology:epsilon_provenance(dignified_death__autonomy_primary, 0.52, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dignified_death__autonomy_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(dignified_death__autonomy_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(dignified_death__autonomy_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness sits in the 0.52-0.60 band as specified by the structural delta: this captures both full state prohibition (highest end) and regulated-but-gatekept regimes (lower end, since some individuals do achieve exit). It declines modestly over the interval as more jurisdictions liberalize eligibility criteria and reduce waiting periods in response to litigation and public pressure. Suppression starts high (0.78) reflecting the criminal-law backdrop against unauthorized hastening of death, and declines slowly (0.68) as legal recognition expands, but remains substantial because even legalized regimes actively criminalize deviation from the sanctioned procedural path (e.g. unauthorized assistance, self-administration outside protocol). Theater ratio rises over time (0.30 to 0.40) as eligibility review procedures accumulate additional certification layers, second-opinion requirements, and psychiatric evaluation steps whose primary function increasingly is legal insulation for physicians and institutions rather than protecting patient welfare — a Goodhart-style drift where the coordination proxy (safety review) grows disproportionately to the underlying risk it manages. Accessibility collapse (0.60) reflects that once a jurisdiction commits to the medical-gatekeeping model, alternative models (e.g. simple witnessed-consent without terminal-diagnosis requirement) become politically foreclosed as 'unsafe' by comparison — the specific procedural form entrenches itself. Resistance (0.70) is high because this reading is actively contested from multiple directions: sanctity-primary advocates resist any legalization, and disability-rights/relational-autonomy advocates resist the individualist framing itself.
 *
 * DIRECTIONALITY LOGIC:
 *   Eligible terminal patients who successfully navigate the eligibility process are the clearest beneficiaries — low d, the arrangement functions for them as designed. Suffering patients denied, delayed, or categorically excluded (non-terminal, prohibited jurisdictions) are targets — high d, trapped exit, the constraint's costs land on them with no institutional recourse. Medical boards and licensing authorities occupy a beneficiary/agenda-setter dual position: they do not suffer under the constraint but derive institutional authority and legitimacy from administering it, which is why their d sits toward the beneficiary end despite bearing none of the underlying suffering. Physicians are structurally distinct: they administer the gatekeeping (agenda-setter function) but bear real legal and psychological cost (payer function) without receiving the dignity benefit — a genuine dual role, not a beneficiary in disguise.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — legally mandated prolongation of unwanted suffering with no exit — is corroborated as at least partially live by courts and legislative testimony, which is why this is not classified as a pure snare or as fully resolved mandatrophy. But the eligibility apparatus built to solve it for terminal patients has, per the disability-rights and non-terminal-patient corroboration, generated a narrower problem of its own: an administrable regime whose gatekeeping now serves institutional legitimacy functions (theater_ratio rising) beyond what patient safety alone would require. The tangled_rope classification holds precisely this tension without collapsing it into either 'pure coordination, unambiguously good' or 'pure extraction, should be abolished' — the coordination function (bounded, auditable exit) and the extraction function (institutional capture of legitimacy, categorical exclusion of non-terminal claimants) are both real and are the same structure viewed by different affected parties.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_choice_autonomy_primary,
    'Is dignity in end-of-life decisions best located in individual self-determination (this reading), in the intrinsic value of life itself (sanctity_primary), or in a relational process distributed across patient-family-clinician (relational_autonomy)? The choice of reading determines who counts as victim, who counts as beneficiary, and whether state prohibition or medical gatekeeping is the primary extraction site.',
    'This is not empirically resolvable — it is a foundational normative commitment about the locus of dignity. What can be documented is which reading a given jurisdiction''s law encodes, and how outcomes differ (rates of assisted death, rates of unaddressed suffering, rates of documented coercion) under each encoded reading, which provides indirect evidence relevant to but not dispositive of the underlying value question.',
    'Under sanctity_primary, this entire arrangement (autonomy_primary''s legalized regime) would itself be classified as extractive on the exactly opposite population — physicians and society coerced into complicity with what that reading treats as impermissible killing. Under relational_autonomy, the pure-individual-authority framing this story assumes would itself be read as under-protective of vulnerable patients pressured by family or economic circumstance, shifting the victim set to include coerced ''choosers.'' The reading selected changes which population is named victim and which is named beneficiary — the structural delta is not a refinement but a reallocation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_choice_autonomy_primary, preference, 'Which of the three sibling kernel readings correctly locates the site of dignity — a foundational value disagreement, not an empirical one.').

omega_variable(
    eligibility_line_naturalness,
    'Is the terminal-diagnosis eligibility boundary a principled application of the autonomy_primary logic, or an unprincipled political compromise that the reading''s own premises do not actually support (since a non-terminal chronic sufferer''s self-determination claim is structurally identical)?',
    'Compare jurisdictions that have extended eligibility beyond terminal diagnosis (e.g., psychiatric suffering, non-terminal chronic conditions) against those that have not, tracking whether outcomes (coercion rates, regret rates, procedural safety) diverge in ways that would justify the boundary on the reading''s own autonomy-based terms rather than on borrowed sanctity-based caution.',
    'If the boundary cannot be justified on autonomy grounds alone, the eligibility apparatus is doing extraction work disguised as safety work — supporting a stronger snare-like reading of the gatekeeping layer. If it can be justified (e.g., genuine capacity-assessment difficulty in non-terminal chronic suffering), the tangled_rope''s coordination component is more substantial than the extraction component suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(eligibility_line_naturalness, conceptual, 'Whether the terminal-diagnosis eligibility line is principled under autonomy_primary''s own logic or borrowed from a different (sanctity-flavored) precautionary framework.').

omega_variable(
    theater_versus_genuine_safeguard,
    'Is the rising theater_ratio in eligibility review procedures genuine safety infrastructure responding to real coercion risk, or institutional self-protection that has decoupled from patient welfare?',
    'Track documented coercion or error cases per capita against procedural complexity over time; if coercion/error rates are flat or declining while procedural steps multiply, the marginal procedure is theater rather than safeguard.',
    'If theater, the gatekeeping apparatus''s legitimacy claim (protecting vulnerable patients) is substantially decorative, strengthening the case that excluded non-terminal patients bear costs of a structure that no longer earns its restrictiveness through actual risk reduction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theater_versus_genuine_safeguard, empirical, 'Whether accumulating eligibility-review procedure is functional safeguard or institutional theater.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dignified_death__autonomy_primary, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dign_tr_t0, dignified_death__autonomy_primary, theater_ratio, 0, 0.3).
narrative_ontology:measurement(dign_tr_t6, dignified_death__autonomy_primary, theater_ratio, 6, 0.33).
narrative_ontology:measurement(dign_tr_t12, dignified_death__autonomy_primary, theater_ratio, 12, 0.35).
narrative_ontology:measurement(dign_tr_t18, dignified_death__autonomy_primary, theater_ratio, 18, 0.37).
narrative_ontology:measurement(dign_tr_t24, dignified_death__autonomy_primary, theater_ratio, 24, 0.39).
narrative_ontology:measurement(dign_tr_t30, dignified_death__autonomy_primary, theater_ratio, 30, 0.4).

% Extraction over time
narrative_ontology:measurement(dign_be_t0, dignified_death__autonomy_primary, base_extractiveness, 0, 0.58).
narrative_ontology:measurement(dign_be_t6, dignified_death__autonomy_primary, base_extractiveness, 6, 0.56).
narrative_ontology:measurement(dign_be_t12, dignified_death__autonomy_primary, base_extractiveness, 12, 0.55).
narrative_ontology:measurement(dign_be_t18, dignified_death__autonomy_primary, base_extractiveness, 18, 0.54).
narrative_ontology:measurement(dign_be_t24, dignified_death__autonomy_primary, base_extractiveness, 24, 0.53).
narrative_ontology:measurement(dign_be_t30, dignified_death__autonomy_primary, base_extractiveness, 30, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(dign_su_t0, dignified_death__autonomy_primary, suppression_requirement, 0, 0.78).
narrative_ontology:measurement(dign_su_t6, dignified_death__autonomy_primary, suppression_requirement, 6, 0.75).
narrative_ontology:measurement(dign_su_t12, dignified_death__autonomy_primary, suppression_requirement, 12, 0.73).
narrative_ontology:measurement(dign_su_t18, dignified_death__autonomy_primary, suppression_requirement, 18, 0.71).
narrative_ontology:measurement(dign_su_t24, dignified_death__autonomy_primary, suppression_requirement, 24, 0.69).
narrative_ontology:measurement(dign_su_t30, dignified_death__autonomy_primary, suppression_requirement, 30, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dignified_death__autonomy_primary, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(dignified_death__autonomy_primary, 0.12).
narrative_ontology:affects_constraint(dignified_death__autonomy_primary, dignified_death__sanctity_primary).
narrative_ontology:affects_constraint(dignified_death__autonomy_primary, dignified_death__relational_autonomy).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the dignified_death kernel, decomposed per the ε-invariance principle because the three readings assign structurally different beneficiary/victim sets and different ε values to what a single colloquial label ('the right to die debate') would otherwise conflate. autonomy_primary assigns high ε (0.52-0.58) to state prohibition and gatekeeping-heavy regimes, naming denied/delayed patients as victims and the autonomous agent as beneficiary. sanctity_primary and relational_autonomy are expected to assign different ε values and different victim sets to the same underlying institutional facts, because they locate dignity differently. All three link to each other via affects_constraints; none averages over the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
