% ============================================================================
% CONSTRAINT STORY: dignified_death__autonomy_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
 *   human_readable: Autonomy-Primary Reading of the Dignified Death Kernel: Self-Determination Over Timing and Method of Death
 *   domain: bioethics/medical_law/political_philosophy
 *
 * SUMMARY:
 *   This story instantiates the autonomy-primary reading of the
 *   dignified-death kernel: dignity is located in self-determination, and the
 *   suffering individual holds final authority over the timing and method of
 *   their own death. Under this reading, state prohibition of assisted dying
 *   is read as a direct extraction from the suffering individual — prolonging
 *   life against will is the harm, not a protection. But the reading does not
 *   eliminate gatekeeping; it relocates it into eligibility criteria
 *   (diagnosis, prognosis window, capacity assessment) administered by
 *   medical and legal institutions. The result is a tangled rope: a genuine
 *   coordination function (a safe, regulated exit pathway replacing unsafe
 *   covert alternatives) entangled with an asymmetric extraction (those who
 *   fail the eligibility gate are prolonged against their will by the same
 *   apparatus that grants exit to others). This is a distinct constraint from
 *   the sanctity-primary reading (which denies any legitimate exit authority
 *   exists) and the relational-autonomy reading (which distributes authority
 *   across patient-family-clinician triad rather than vesting it in the
 *   individual) — per the ε-invariance principle, each reading is authored as
 *   its own constraint with its own ε, beneficiary/victim structure, and
 *   type.
 *
 * KEY AGENTS:
 *   - autonomous_dying_patients_who_qualify: primary beneficiary, moderate power, constrained exit — gains legal pathway but only within eligibility bounds
 *   - terminally_suffering_patients_denied_eligibility: primary victim, powerless, trapped — extraction target of the eligibility boundary itself
 *   - patients_in_prohibition_jurisdictions: secondary victim, powerless, trapped — extraction target of outright state prohibition
 *   - compliant_medical_providers: agenda-setter, institutional power, mobile exit — administers the eligibility gate and can decline participation at low cost
 *   - disability_rights_advocates: excluded voice, organized but structurally discounted in criteria design
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
narrative_ontology:constraint_metric(dignified_death__autonomy_primary, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(dignified_death__autonomy_primary, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dignified_death__autonomy_primary, tangled_rope).
narrative_ontology:human_readable(dignified_death__autonomy_primary, "Autonomy-Primary Reading of the Dignified Death Kernel: Self-Determination Over Timing and Method of Death").
narrative_ontology:topic_domain(dignified_death__autonomy_primary, "bioethics/medical_law/political_philosophy").

domain_priors:requires_active_enforcement(dignified_death__autonomy_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dignified_death__autonomy_primary, '0c657bc6-603f-42d4-b4de-fa1a3c316a32').
narrative_ontology:cs_kernel_codification('0c657bc6-603f-42d4-b4de-fa1a3c316a32', distributed).
narrative_ontology:cs_authority_grounding('0c657bc6-603f-42d4-b4de-fa1a3c316a32', distributed).
narrative_ontology:cs_reading_relation('0c657bc6-603f-42d4-b4de-fa1a3c316a32', dignified_death__sanctity_primary, forecloses).
narrative_ontology:cs_reading_relation('0c657bc6-603f-42d4-b4de-fa1a3c316a32', dignified_death__relational_autonomy, influences).
narrative_ontology:cs_axiom('0c657bc6-603f-42d4-b4de-fa1a3c316a32', foundational, individual_consent_sufficient_for_death_timing).
narrative_ontology:cs_axiom_status(individual_consent_sufficient_for_death_timing, holdable).
narrative_ontology:cs_axiom_grounding('0c657bc6-603f-42d4-b4de-fa1a3c316a32', individual_consent_sufficient_for_death_timing, deontological).
narrative_ontology:cs_axiom('0c657bc6-603f-42d4-b4de-fa1a3c316a32', secondary, prolonged_unwanted_suffering_constitutes_harm).
narrative_ontology:cs_axiom_status(prolonged_unwanted_suffering_constitutes_harm, holdable).
narrative_ontology:cs_axiom_grounding('0c657bc6-603f-42d4-b4de-fa1a3c316a32', prolonged_unwanted_suffering_constitutes_harm, empirically_contingent).
narrative_ontology:cs_reference_frame('0c657bc6-603f-42d4-b4de-fa1a3c316a32', individual_sovereignty_over_bodily_fate).
narrative_ontology:cs_drift_state('0c657bc6-603f-42d4-b4de-fa1a3c316a32', post_legalization_wave_contemporary, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('0c657bc6-603f-42d4-b4de-fa1a3c316a32', '').
narrative_ontology:cs_kernel_id(dignified_death__autonomy_primary, dignified_death).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dignified_death__autonomy_primary, autonomous_dying_patients_who_qualify).
narrative_ontology:constraint_beneficiary(dignified_death__autonomy_primary, right_to_die_advocacy_organizations).
narrative_ontology:constraint_beneficiary(dignified_death__autonomy_primary, compliant_medical_providers).
narrative_ontology:constraint_victim(dignified_death__autonomy_primary, terminally_suffering_patients_denied_eligibility).
narrative_ontology:constraint_victim(dignified_death__autonomy_primary, patients_in_prohibition_jurisdictions).
narrative_ontology:constraint_victim(dignified_death__autonomy_primary, patients_excluded_by_capacity_or_diagnosis_criteria).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Terminally ill or intolerably suffering individuals who meet a jurisdiction's eligibility criteria (diagnosis, prognosis window, capacity assessment, waiting periods) and are permitted to request and receive medical assistance in dying. They experience the arrangement as a genuine expansion of control over their final days, but their exit is gated behind procedural qualification, not pure self-determination.
narrative_ontology:constraint_stakeholder(dignified_death__autonomy_primary, autonomous_dying_patients_who_qualify, beneficiary,
    moderate, immediate, constrained, national).

% Individuals suffering comparably to those who qualify, but excluded by criteria such as prognosis-window requirements, non-terminal chronic conditions, contested capacity assessments, or diagnosis categories the framework does not recognize (e.g. purely psychiatric suffering). They are prolonged against their expressed will by the same eligibility apparatus that grants exit to others, and bear the cost of a boundary they did not draw.
narrative_ontology:constraint_stakeholder(dignified_death__autonomy_primary, terminally_suffering_patients_denied_eligibility, payer,
    powerless, immediate, trapped, national).

% Suffering individuals living where assisted dying remains criminalized entirely. Under the autonomy-primary reading, state prohibition here is read as a direct denial of final authority over one's own death, with the highest measured extractiveness of any actor in the story — they have no legal exit at all and must travel, suffer, or act covertly.
narrative_ontology:constraint_stakeholder(dignified_death__autonomy_primary, patients_in_prohibition_jurisdictions, payer,
    powerless, immediate, trapped, national).

% Physicians and institutions that administer eligibility assessments, certify diagnoses and capacity, and ultimately authorize or perform the assisted-dying procedure. They gatekeep access under legal and professional liability, which gives them real discretionary power over who exits and who is deemed not-yet-eligible; they can also decline to participate via conscience exemptions with little personal cost.
narrative_ontology:constraint_stakeholder(dignified_death__autonomy_primary, compliant_medical_providers, agenda_setter,
    institutional, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(dignified_death__autonomy_primary, compliant_medical_providers, beneficiary).

% Organizations that campaigned for and now defend autonomy-primary legislation. They benefit reputationally and organizationally from the framework's existence and expansion, and shape eligibility-criteria debates, but do not themselves bear the suffering the framework governs.
narrative_ontology:constraint_stakeholder(dignified_death__autonomy_primary, right_to_die_advocacy_organizations, beneficiary,
    organized, generational, mobile, national).

% Groups who argue that autonomy-primary frameworks create disparate pressure on disabled and chronically ill people to choose death under social, financial, or caregiving-burden pressures that are not truly autonomous. Their objections are frequently raised in legislative hearings but structurally discounted in eligibility-criteria design, which is authored primarily around individual-choice framing.
narrative_ontology:constraint_stakeholder(dignified_death__autonomy_primary, disability_rights_advocates, excluded,
    organized, generational, constrained, national).

% Bodies that write and adjudicate the eligibility criteria, define capacity standards, and set the boundary between permitted and prohibited exit. They receive testimony from all other seats and can expand or contract the eligibility apparatus, but do not themselves suffer or benefit directly from any individual case.
narrative_ontology:constraint_stakeholder(dignified_death__autonomy_primary, state_legislatures_and_courts, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(dignified_death__autonomy_primary, diffuse).
narrative_ontology:fixing_cost_class(dignified_death__autonomy_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a legally recognized, medically supervised pathway for competent, suffering individuals to end their lives on their own terms, replacing ad hoc, unsafe, or covert self-termination with a regulated procedure that protects both patient intent and provider liability.
% TRANSFER_FUNCTION: Moves final authority over the timing and method of death from the state/medical establishment's default presumption of preserving life at all costs to the individual patient — but only for those who pass through an eligibility gate; those who fail the gate have that authority transferred back to institutional discretion, effectively at the cost of their continued, unwanted suffering.
% ABSENT_VOICES: Patients excluded by narrow prognosis windows (e.g. chronic non-terminal suffering, psychiatric-only suffering) and disability-rights advocates who argue the autonomy framing masks social coercion are structurally present in public debate but rarely determinative in how eligibility criteria are drawn, which remains authored primarily by legislatures, courts, and advocacy organizations centered on the paradigmatic terminal-cancer case.
% DISAPPEARANCE_RATIONALE: If the autonomy-primary legal framework vanished overnight, currently-eligible patients would lose their legal pathway to assisted dying entirely, providers would face renewed liability for any assistance, and the population currently exiting through medical channels would revert to unsupervised self-termination, covert assistance, or prolonged unwanted suffering — a substantial, observable rearrangement of end-of-life practice.
% FOUNDING_PROBLEM: Individuals suffering unbearably from terminal or severe conditions had no legal, medically supervised way to control the timing and manner of their death; the alternatives were prolonged suffering against their will, unsafe self-administered attempts, or criminalized covert assistance from loved ones or physicians.
% FOUNDING_PROBLEM_CORROBORATION: Right-to-die advocacy organizations and qualifying patients attest the founding problem remains live and the framework directly answers it. Disability-rights advocates and some bioethicists outside the advocacy coalition attest that the framework has partially shifted function — from relieving involuntary suffering to legitimizing exit under social and caregiving-burden pressures that are not autonomy in the pure sense the framework claims — a critique corroborated in part by clinical ethics literature documenting eligibility-boundary disputes.
narrative_ontology:disappearance_verdict(dignified_death__autonomy_primary, world_rearranges).
narrative_ontology:founding_problem_status(dignified_death__autonomy_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dignified_death__autonomy_primary, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
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
 *   Extractiveness is authored at 0.52, near the top of the story's expected 0.45-0.60 band, reflecting that under this reading state prohibition and narrow eligibility gating both constitute genuine extraction from suffering individuals denied exit — but the extraction is partially offset by the framework's real coordination function for those who do qualify, which is why ε sits mid-band rather than at a snare-level ceiling. Suppression is authored higher (0.68) than extractiveness because the mechanism holding the boundary in place — capacity assessments, prognosis-window rules, professional liability regimes, criminal law in prohibition jurisdictions — is a substantial active apparatus independent of how much value it ultimately extracts. Theater ratio is moderate (0.40) and rising over the interval: as eligibility litigation and criteria-refinement accumulate, an increasing share of institutional activity is procedural compliance and defensive documentation rather than direct relief of suffering. The declining extractiveness and suppression trend across the interval reflects the reading's own historical trajectory — eligibility criteria have gradually loosened in jurisdictions that adopted assisted-dying law, narrowing (without eliminating) the victim set.
 *
 * PERSPECTIVAL GAP:
 *   From the qualifying patient's seat, the arrangement looks like a rope: real coordination, real benefit, minimal residual cost. From the denied-eligibility patient's seat, the identical eligibility apparatus looks like a snare: suffering is prolonged by a bureaucratic line that could have been drawn to include them and was not. The engine computes these divergent seat classifications from the same structural data; the story's claimed type (tangled_rope) is the whole-constraint reading that holds both computations as simultaneously true rather than averaging them away.
 *
 * DIRECTIONALITY LOGIC:
 *   Qualifying patients and advocacy organizations are declared beneficiaries because the framework's operation subsidizes their goal (legal exit, legislative legitimacy) at low structural cost to them. Denied-eligibility patients and prohibition-jurisdiction patients are declared victims because the same apparatus that grants exit to others actively withholds it from them — their d sits near the full-target end because they are trapped (no legal alternative) and their suffering is the mechanism's direct cost. Medical providers are agenda-setters with mobile exit (conscience exemptions, ability to decline) which keeps their directionality well short of victim status despite bearing legal liability.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — unsupervised or covert self-termination in the absence of any legal pathway — is only partially resolved by the current apparatus: it is live for prohibition-jurisdiction patients and denied-eligibility patients, and arguably dead (resolved) for the qualifying-patient population the law was drafted around. Classifying this as tangled_rope rather than snare prevents mislabeling the genuine coordination benefit to qualifying patients as pure extraction; classifying it as tangled_rope rather than rope prevents mislabeling the asymmetric cost borne by excluded populations as a mere externality of an otherwise clean coordination mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    eligibility_boundary_as_extraction_or_prudence,
    'Is the eligibility gate (prognosis window, diagnosis category, capacity assessment) a genuine safeguard against irreversible error, or is it an extraction mechanism that arbitrarily withholds autonomy from populations the framework was never designed to include?',
    'Comparative outcome studies across jurisdictions with looser vs. stricter eligibility criteria, tracking rates of regret, coercion, and unmet requests for assistance among excluded populations.',
    'If the boundary tracks genuine risk of error or coercion, the excluded population''s classification shifts toward a residual coordination cost rather than a victim class; if the boundary tracks institutional risk-aversion or moral discomfort unrelated to actual risk, the tangled_rope classification''s victim component strengthens toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(eligibility_boundary_as_extraction_or_prudence, empirical, 'Whether eligibility criteria function as safeguard or as arbitrary extraction boundary.').

omega_variable(
    state_prohibition_as_mountain_or_snare,
    'In jurisdictions with outright prohibition, is the prohibition better modeled as this same tangled-rope constraint''s most extractive edge case, or as a structurally distinct snare constraint (no coordination function at all, pure extraction from the suffering individual)?',
    'Assess whether prohibition jurisdictions retain any residual coordination function (e.g. suicide-prevention infrastructure serving a broader population) distinguishable from the assisted-dying-specific prohibition; if none exists, decompose per the ε-invariance principle into a separate story.',
    'If prohibition has no coordination function of its own, it should be authored as a separate snare constraint rather than folded into this tangled_rope''s high-extraction tail, per the decomposition discipline for observable-dependent ε values.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(state_prohibition_as_mountain_or_snare, conceptual, 'Whether outright state prohibition belongs inside this constraint or is a separate, more extractive sibling constraint.').

omega_variable(
    capacity_assessment_reliability,
    'How reliably do capacity assessments distinguish genuinely autonomous, uncoerced requests from requests shaped by untreated depression, inadequate palliative care access, or caregiving-burden pressure?',
    'Longitudinal clinical studies comparing capacity-assessment outcomes against post-hoc psychiatric review and against palliative-care-access controls.',
    'Low reliability would mean some fraction of the ''beneficiary'' population (qualifying patients) is misclassified — their apparent autonomous exit may itself carry a hidden victim structure, which would reduce the framework''s claimed coordination-benefit and push the classification further toward extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(capacity_assessment_reliability, empirical, 'Whether the capacity gate reliably isolates genuine autonomy from coerced or clinically confounded requests.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dignified_death__autonomy_primary, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dign_tr_t0, dignified_death__autonomy_primary, theater_ratio, 0, 0.3).
narrative_ontology:measurement(dign_tr_t6, dignified_death__autonomy_primary, theater_ratio, 6, 0.32).
narrative_ontology:measurement(dign_tr_t12, dignified_death__autonomy_primary, theater_ratio, 12, 0.35).
narrative_ontology:measurement(dign_tr_t18, dignified_death__autonomy_primary, theater_ratio, 18, 0.37).
narrative_ontology:measurement(dign_tr_t24, dignified_death__autonomy_primary, theater_ratio, 24, 0.39).
narrative_ontology:measurement(dign_tr_t30, dignified_death__autonomy_primary, theater_ratio, 30, 0.4).

% Extraction over time
narrative_ontology:measurement(dign_be_t0, dignified_death__autonomy_primary, base_extractiveness, 0, 0.58).
narrative_ontology:measurement(dign_be_t6, dignified_death__autonomy_primary, base_extractiveness, 6, 0.56).
narrative_ontology:measurement(dign_be_t12, dignified_death__autonomy_primary, base_extractiveness, 12, 0.55).
narrative_ontology:measurement(dign_be_t18, dignified_death__autonomy_primary, base_extractiveness, 18, 0.53).
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
% This story is one of three linked readings of the dignified_death kernel. autonomy_primary (this story) authors ε=0.52 with victims defined as those denied exit; sanctity_primary authors a structurally opposed premise where any intentional life-termination is the harm regardless of consent, with a correspondingly different victim/beneficiary structure; relational_autonomy distributes decision authority across a patient-family-clinician triad rather than vesting it solely in the individual, producing a different eligibility apparatus and different extraction profile. All three share the same underlying medical-legal domain but are authored as distinct constraints per the ε-invariance principle, since the natural-language label 'right to die' or 'dignified death' conflates structurally incompatible claims about where final authority resides.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
