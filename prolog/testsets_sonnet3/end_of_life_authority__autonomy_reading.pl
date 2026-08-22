% ============================================================================
% CONSTRAINT STORY: end_of_life_authority__autonomy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: end_of_life_authority__autonomy_reading
 *   human_readable: Autonomy-Grounded Right to Medically Assisted Death
 *   domain: medical/legal/ethical
 *
 * SUMMARY:
 *   This story authors the autonomy reading of the end_of_life_authority
 *   kernel: the claim that individual autonomy grounds a right to control the
 *   circumstances and timing of one's death when facing unbearable suffering.
 *   Under this reading, the standing arrangement under contest is the CURRENT
 *   legal prohibition/restriction regime that this reading contests and seeks
 *   to loosen — the reading's ε is authored against that standing
 *   arrangement's extractiveness on suffering patients denied choice, not
 *   against the hypothetical fully-realized autonomy regime. The suppression
 *   metric is authored high (0.62 falling to a still-substantial level)
 *   because paternalistic restrictions actively suppress the exercise of this
 *   claimed right, and that suppression is the primary structural fact this
 *   reading identifies. This is one of three linked readings of the same
 *   kernel; the sanctity_reading and slippery_slope_mechanism readings are
 *   separate constraint stories with their own ε values, stakeholders, and
 *   structural claims — they are not folded into this one.
 *
 * KEY AGENTS:
 *   - competent_terminally_ill_patients: primary beneficiary under this reading — gains legal control over death timing
 *   - suffering_patients_denied_access: bears the cost of the reading's own eligibility boundary — victim within the autonomy framework itself
 *   - attending_physicians: agenda-setter/gatekeeper who operationalizes the right in clinical practice
 *   - legislatures_and_courts: institutional agenda-setter drawing and revising eligibility lines
 *   - disability_rights_advocates: excluded voice objecting to the framing itself
 *   - palliative_care_institutions: analytical observer whose funding adequacy shapes whether 'choice' is genuine
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(end_of_life_authority__autonomy_reading, 0.28).
domain_priors:suppression_score(end_of_life_authority__autonomy_reading, 0.62).
domain_priors:theater_ratio(end_of_life_authority__autonomy_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(end_of_life_authority__autonomy_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(end_of_life_authority__autonomy_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(end_of_life_authority__autonomy_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(end_of_life_authority__autonomy_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(end_of_life_authority__autonomy_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(end_of_life_authority__autonomy_reading, rope).
narrative_ontology:human_readable(end_of_life_authority__autonomy_reading, "Autonomy-Grounded Right to Medically Assisted Death").
narrative_ontology:topic_domain(end_of_life_authority__autonomy_reading, "medical/legal/ethical").

domain_priors:requires_active_enforcement(end_of_life_authority__autonomy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(end_of_life_authority__autonomy_reading, 'db2e0117-2994-41b3-b5f2-7396047ec404').
narrative_ontology:cs_kernel_codification('db2e0117-2994-41b3-b5f2-7396047ec404', distributed).
narrative_ontology:cs_authority_grounding('db2e0117-2994-41b3-b5f2-7396047ec404', distributed).
narrative_ontology:cs_reading_relation('db2e0117-2994-41b3-b5f2-7396047ec404', end_of_life_authority__sanctity_reading, coexists_with).
narrative_ontology:cs_reading_relation('db2e0117-2994-41b3-b5f2-7396047ec404', end_of_life_authority__slippery_slope_mechanism, influences).
narrative_ontology:cs_axiom('db2e0117-2994-41b3-b5f2-7396047ec404', foundational, bodily_autonomy_extends_to_timing_of_death).
narrative_ontology:cs_axiom_status(bodily_autonomy_extends_to_timing_of_death, holdable).
narrative_ontology:cs_axiom_grounding('db2e0117-2994-41b3-b5f2-7396047ec404', bodily_autonomy_extends_to_timing_of_death, deontological).
narrative_ontology:cs_axiom('db2e0117-2994-41b3-b5f2-7396047ec404', secondary, unbearable_suffering_overrides_categorical_prohibition).
narrative_ontology:cs_axiom_status(unbearable_suffering_overrides_categorical_prohibition, holdable).
narrative_ontology:cs_axiom_grounding('db2e0117-2994-41b3-b5f2-7396047ec404', unbearable_suffering_overrides_categorical_prohibition, instrumental).
narrative_ontology:cs_reference_frame('db2e0117-2994-41b3-b5f2-7396047ec404', common_law_prohibition_on_assisted_suicide).
narrative_ontology:cs_drift_state('db2e0117-2994-41b3-b5f2-7396047ec404', contemporary_legalization_wave, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('db2e0117-2994-41b3-b5f2-7396047ec404', '').
narrative_ontology:cs_kernel_id(end_of_life_authority__autonomy_reading, end_of_life_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(end_of_life_authority__autonomy_reading, competent_terminally_ill_patients).
narrative_ontology:constraint_beneficiary(end_of_life_authority__autonomy_reading, families_of_dying_patients).
narrative_ontology:constraint_victim(end_of_life_authority__autonomy_reading, suffering_patients_denied_access).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(end_of_life_authority__autonomy_reading, attending_physicians).
narrative_ontology:constraint_victim(end_of_life_authority__autonomy_reading, palliative_care_institutions).
narrative_ontology:constraint_vindicates(end_of_life_authority__autonomy_reading, bodily_autonomy_extends_to_death_timing).
narrative_ontology:constraint_vindicates(end_of_life_authority__autonomy_reading, unbearable_suffering_is_a_legitimate_medical_endpoint).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Face a terminal diagnosis with a defined prognosis and unbearable suffering they judge intolerable. Where the autonomy framework is legally recognized, they can request a controlled, physician-assisted death after meeting eligibility criteria (competence, terminality, repeated request, waiting period). Their exit from suffering depends entirely on jurisdiction; crossing a border to access the right is often the only alternative to enduring it.
narrative_ontology:constraint_stakeholder(end_of_life_authority__autonomy_reading, competent_terminally_ill_patients, beneficiary,
    moderate, immediate, constrained, regional).

% Experience the same unbearable, irreversible suffering but fall outside eligibility (non-terminal chronic illness, psychiatric suffering, incapacity to self-administer, inability to travel) or live in a jurisdiction where the right does not exist. They bear the cost of the autonomy framework's boundary lines directly, in continued suffering, with no legal exit.
narrative_ontology:constraint_stakeholder(end_of_life_authority__autonomy_reading, suffering_patients_denied_access, payer,
    powerless, immediate, trapped, regional).

% Assess eligibility, administer or prescribe the lethal medication, and bear legal and professional liability for the determination. They operationalize what 'unbearable suffering' and 'competence' mean in practice, effectively setting the boundary of the right through clinical judgment. Conscientious objectors can decline individual cases but operate within a system they did not design.
narrative_ontology:constraint_stakeholder(end_of_life_authority__autonomy_reading, attending_physicians, agenda_setter,
    organized, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(end_of_life_authority__autonomy_reading, attending_physicians, beneficiary).

% Argue that framing assisted death as autonomy exercised by disabled or chronically ill people encodes a social judgment that their lives are less worth living, and that inadequate palliative and disability support pushes people toward the exit rather than genuinely choosing it. Their objections are raised in legislative hearings and litigation but rarely control the eligibility criteria as written.
narrative_ontology:constraint_stakeholder(end_of_life_authority__autonomy_reading, disability_rights_advocates, excluded,
    organized, generational, constrained, national).

% Write and adjudicate the statutory eligibility criteria — who qualifies, what safeguards apply, how competence is verified. They respond to litigation pressure from both directions: patients seeking wider access and advocates seeking narrower or no access. Their decisions set the boundary the entire framework operates within.
narrative_ontology:constraint_stakeholder(end_of_life_authority__autonomy_reading, legislatures_and_courts, agenda_setter,
    institutional, generational, analytical, national).

% Provide the alternative to assisted death — pain management, hospice, comfort care. Where palliative resources are underfunded, patients' 'choice' of assisted death is shaped by the absence of a genuine alternative, which some in this seat regard as a structural cost imposed on them by the autonomy framework's political success.
narrative_ontology:constraint_stakeholder(end_of_life_authority__autonomy_reading, palliative_care_institutions, observer,
    organized, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(end_of_life_authority__autonomy_reading, palliative_care_institutions, payer).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(end_of_life_authority__autonomy_reading, diffuse).
narrative_ontology:fixing_cost_class(end_of_life_authority__autonomy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a legally regulated, physician-mediated process for competent, suffering patients to end their own lives on their own timeline, replacing unregulated self-harm, botched suicide attempts, or indefinite prolongation of suffering against the patient's will.
% TRANSFER_FUNCTION: Moves the authority to determine the timing and manner of death from medical/legal paternalism (which previously prohibited it categorically) to the individual patient, mediated by physician gatekeeping and statutory eligibility criteria.
% ABSENT_VOICES: Patients who fall just outside the eligibility line — chronic non-terminal sufferers, psychiatric patients, incapacitated patients unable to self-administer — are the clearest absent voices: they experience the suffering the framework was built to address but are excluded from its remedy by the boundary lines legislatures drew. Disability rights advocates raise this in testimony but do not control the eligibility text.
% DISAPPEARANCE_RATIONALE: If the autonomy-grounded right vanished overnight, currently-eligible patients would lose legal access to assisted death entirely, reverting to either indefinite suffering, unregulated self-harm, or cross-border travel where available. Physicians would lose the legal shield for participation. The palliative care system would absorb all end-of-life demand by default rather than by patient choice.
% FOUNDING_PROBLEM: Terminally ill, competent patients experiencing unbearable and irremediable suffering had no legal, medically supervised way to control the timing of their death; the only options were enduring suffering against their will, unregulated and often violent self-harm, or covert physician assistance without legal protection or oversight.
% FOUNDING_PROBLEM_CORROBORATION: Patient advocacy groups and many treating physicians attest the founding problem remains live and the framework addresses it as intended for the population it covers. Disability rights organizations and some palliative care physicians, from outside the framework's beneficiary set, attest that the problem has been reframed rather than solved: inadequate care infrastructure and expanding eligibility criteria suggest the arrangement is drifting from relieving terminal suffering toward becoming a substitute for social and medical support that was never adequately funded.
narrative_ontology:disappearance_verdict(end_of_life_authority__autonomy_reading, world_rearranges).
narrative_ontology:founding_problem_status(end_of_life_authority__autonomy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(end_of_life_authority__autonomy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(end_of_life_authority__autonomy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(end_of_life_authority__autonomy_reading, 0.28, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(end_of_life_authority__autonomy_reading_tests).
:- end_tests(end_of_life_authority__autonomy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low-to-moderate (0.28 by interval end) because, under the autonomy reading's own lights, the standing paternalistic restriction extracts from patients by denying them a genuinely low-extraction remedy — the extraction is the cost of denial, not the cost of the right's exercise. Suppression is authored high (0.62-0.78 across the interval) because the central structural claim of this reading is that paternalistic legal and medical restrictions actively suppress the autonomy claim; this suppression is trending downward over the interval as jurisdictions increasingly recognize the right, which is exactly what this reading's proponents would expect to observe if their reading is correct. Accessibility collapse is moderate (0.35) — alternatives (suicide, unregulated euthanasia, cross-border travel) persist even where the right is denied, so collapse is not total. Resistance is moderate-high (0.55) reflecting active opposition from sanctity-of-life advocates and disability rights groups.
 *
 * PERSPECTIVAL GAP:
 *   From the seat of a currently-eligible, terminally-ill patient, the arrangement looks like liberation — an unambiguous expansion of choice with minimal cost. From the seat of a chronic-pain patient denied eligibility, or a disability rights advocate, the same legal architecture looks like a system that legitimizes ending some lives while implicitly ranking others as less rescuable — the suppression is experienced as exclusion rather than paternalism. The engine computes these as different seat-level classifications from the same structural data; this story does not adjudicate which seat is correct.
 *
 * DIRECTIONALITY LOGIC:
 *   Competent terminally ill patients who qualify are the structural beneficiaries — the reading grants them a low-d position (near beneficiary end) because the arrangement subsidizes their autonomy claim. Suffering patients denied access sit at the high-d target end: they bear the cost of the boundary the reading itself draws, trapped with no legal exit. Physicians and legislatures occupy agenda-setter positions with moderate directionality — they administer the boundary but do not extract rents from it. This is the key seat-divergence fact of the autonomy reading: it produces its OWN victim class (those just outside the eligibility line) even while advancing the coordination story of patient self-determination.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — unbearable, irremediable suffering with no legal exit — remains partially live for excluded populations even as it is resolved for the initially-targeted population (competent terminal patients). This prevents mislabeling the arrangement as either pure coordination (it has left a residual victim class) or pure extraction (a real and substantial beneficiary population exists whose suffering the reading genuinely addresses). The classification as rope-with-victims (which the engine will assess independently) reflects that mixed status rather than forcing a binary verdict.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    autonomy_reading_kernel_disagreement_location,
    'Where exactly does the autonomy reading''s disagreement with the sanctity reading and the slippery-slope reading actually locate — is it a disagreement about facts (does eligibility expand empirically), values (does bodily autonomy extend to death), or both?',
    'Track eligibility-criteria legislative history across jurisdictions that adopted the autonomy framework; separately survey whether opposition is grounded in predicted empirical expansion or in the deontological premise itself.',
    'If the disagreement is purely empirical (expansion trajectory), the slippery_slope_mechanism reading could in principle be resolved by data and would influence, not forecose, the autonomy reading. If it is purely normative (does autonomy extend to death), no amount of data resolves it, and the readings coexist indefinitely across different political coalitions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(autonomy_reading_kernel_disagreement_location, conceptual, 'Locating whether the kernel contest is empirical, normative, or mixed.').

omega_variable(
    genuine_choice_vs_constrained_choice,
    'When a patient ''chooses'' assisted death in a jurisdiction with inadequate palliative care funding, is that a genuine exercise of the autonomy this reading grounds, or is it a choice constrained into existence by the absence of a real alternative?',
    'Compare uptake rates and stated reasons for choosing assisted death across jurisdictions with well-funded versus poorly-funded palliative care infrastructure; if uptake correlates strongly with care scarcity rather than suffering severity, the autonomy framing is partly a cover for austerity.',
    'If choice is substantially constrained by care scarcity, the extractiveness authored here (0.28) understates the true cost borne by patients who ''choose'' death because no adequate alternative was funded — this would push the reading''s own metrics toward tangled_rope territory.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(genuine_choice_vs_constrained_choice, empirical, 'Whether documented patient choice is genuinely autonomous or infrastructure-constrained.').

omega_variable(
    eligibility_boundary_naturalness,
    'Is the specific eligibility boundary (terminal, competent, repeated request) a principled derivation from the autonomy premise, or an arbitrary political compromise that could shift in either direction without contradicting the underlying premise?',
    'Examine whether autonomy-reading proponents defend the specific boundary on principled grounds (only terminal suffering justifies exception) or strategic grounds (this boundary was politically achievable, wider boundaries may follow).',
    'If the boundary is strategic rather than principled, the autonomy reading structurally predicts its own future convergence toward the slippery_slope_mechanism reading''s empirical claim — the two readings would not be independent but temporally sequential.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(eligibility_boundary_naturalness, conceptual, 'Whether the current eligibility line is a stable principled boundary or a staging point.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(end_of_life_authority__autonomy_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(end__tr_t0, end_of_life_authority__autonomy_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(end__tr_t0, observed).
narrative_ontology:measurement(end__tr_t6, end_of_life_authority__autonomy_reading, theater_ratio, 6, 0.09).
narrative_ontology:measurement_basis(end__tr_t6, observed).
narrative_ontology:measurement(end__tr_t12, end_of_life_authority__autonomy_reading, theater_ratio, 12, 0.1).
narrative_ontology:measurement_basis(end__tr_t12, observed).
narrative_ontology:measurement(end__tr_t18, end_of_life_authority__autonomy_reading, theater_ratio, 18, 0.12).
narrative_ontology:measurement_basis(end__tr_t18, observed).
narrative_ontology:measurement(end__tr_t24, end_of_life_authority__autonomy_reading, theater_ratio, 24, 0.14).
narrative_ontology:measurement_basis(end__tr_t24, observed).
narrative_ontology:measurement(end__tr_t30, end_of_life_authority__autonomy_reading, theater_ratio, 30, 0.15).
narrative_ontology:measurement_basis(end__tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(end__be_t0, end_of_life_authority__autonomy_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement_basis(end__be_t0, observed).
narrative_ontology:measurement(end__be_t6, end_of_life_authority__autonomy_reading, base_extractiveness, 6, 0.18).
narrative_ontology:measurement_basis(end__be_t6, observed).
narrative_ontology:measurement(end__be_t12, end_of_life_authority__autonomy_reading, base_extractiveness, 12, 0.21).
narrative_ontology:measurement_basis(end__be_t12, observed).
narrative_ontology:measurement(end__be_t18, end_of_life_authority__autonomy_reading, base_extractiveness, 18, 0.23).
narrative_ontology:measurement_basis(end__be_t18, observed).
narrative_ontology:measurement(end__be_t24, end_of_life_authority__autonomy_reading, base_extractiveness, 24, 0.26).
narrative_ontology:measurement_basis(end__be_t24, observed).
narrative_ontology:measurement(end__be_t30, end_of_life_authority__autonomy_reading, base_extractiveness, 30, 0.28).
narrative_ontology:measurement_basis(end__be_t30, observed).

% Suppression requirement over time
narrative_ontology:measurement(end__su_t0, end_of_life_authority__autonomy_reading, suppression_requirement, 0, 0.78).
narrative_ontology:measurement_basis(end__su_t0, observed).
narrative_ontology:measurement(end__su_t6, end_of_life_authority__autonomy_reading, suppression_requirement, 6, 0.74).
narrative_ontology:measurement_basis(end__su_t6, observed).
narrative_ontology:measurement(end__su_t12, end_of_life_authority__autonomy_reading, suppression_requirement, 12, 0.7).
narrative_ontology:measurement_basis(end__su_t12, observed).
narrative_ontology:measurement(end__su_t18, end_of_life_authority__autonomy_reading, suppression_requirement, 18, 0.67).
narrative_ontology:measurement_basis(end__su_t18, observed).
narrative_ontology:measurement(end__su_t24, end_of_life_authority__autonomy_reading, suppression_requirement, 24, 0.64).
narrative_ontology:measurement_basis(end__su_t24, observed).
narrative_ontology:measurement(end__su_t30, end_of_life_authority__autonomy_reading, suppression_requirement, 30, 0.62).
narrative_ontology:measurement_basis(end__su_t30, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(end_of_life_authority__autonomy_reading, end_of_life_authority__sanctity_reading).
narrative_ontology:affects_constraint(end_of_life_authority__autonomy_reading, end_of_life_authority__slippery_slope_mechanism).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the end_of_life_authority kernel. autonomy_reading (this story) and sanctity_reading share the same underlying text and legal contest but author opposed ε values and disjoint beneficiary/victim sets from the standing restrictive arrangement each reading is about. slippery_slope_mechanism is downstream of this reading in the sense that its empirical claim (eligibility expansion) can only be evaluated once an autonomy-grounded regime exists to expand; it is authored as a separate story because its ε concerns the expansion dynamic itself, not the initial right.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
