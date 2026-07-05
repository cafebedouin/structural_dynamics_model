% ============================================================================
% CONSTRAINT STORY: dignified_death__sanctity_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dignified_death__sanctity_primary, []).

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
 *   constraint_id: dignified_death__sanctity_primary
 *   human_readable: Sanctity-of-Life Prohibition on Assisted Death
 *   domain: bioethics/medical_law/political_philosophy
 *
 * SUMMARY:
 *   This constraint represents the sanctity-primary reading of the
 *   dignified-death kernel: the claim that dignity resides in life's
 *   intrinsic, transcendent value, such that intentional life-termination is
 *   prohibited regardless of individual consent. As comparative jurisdictions
 *   increasingly adopt autonomy-based or relational-autonomy legal frameworks
 *   for medical assistance in dying, the sanctity-primary reading has shifted
 *   from being the unmarked default (pre-1990s, largely uncontested across
 *   Western legal systems) to being an actively defended, increasingly
 *   minority legal position requiring sustained institutional advocacy to
 *   maintain. The rising theater_ratio and suppression_requirement over the
 *   measured interval reflect this shift: what was once background law now
 *   requires active lobbying, litigation defense, and public moral argument
 *   to preserve against a competing reading gaining legal ground. This story
 *   generates ONLY the sanctity-primary reading as a clean, ε-invariant
 *   constraint; the autonomy_primary and relational_autonomy readings are
 *   separate constraint stories with their own ε values, beneficiary/victim
 *   structures, and classifications, linked via network.affects_constraints.
 *   Do not read this file as adjudicating between the readings — it authors
 *   one reading's structure faithfully.
 *
 * KEY AGENTS:
 *   - religious_institutions: primary agenda-setter and doctrinal source, institutional/analytical exit — supplies the moral architecture
 *   - disability_rights_organizations_opposing_legalization: genuine coordination beneficiary with real coercion concerns, organized/constrained — internally divided constituency
 *   - palliative_care_establishment: institutional beneficiary with resource and identity stake in remaining the sole sanctioned pathway
 *   - terminally_ill_patients_seeking_death: primary victim, powerless/trapped, immediate time horizon — bears the direct cost of foreclosure
 *   - elderly_dependents_under_family_pressure: cited as justification for the prohibition while remaining vulnerable to a different, unaddressed form of coercion
 *   - disabled_people_denied_autonomy: victim of paternalistic denial of decision authority extended to non-disabled patients
 *   - legislatures_and_courts: formal agenda-setter with power to change the constraint, subject to sustained organized resistance
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dignified_death__sanctity_primary, 0.58).
domain_priors:suppression_score(dignified_death__sanctity_primary, 0.71).
domain_priors:theater_ratio(dignified_death__sanctity_primary, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dignified_death__sanctity_primary, extractiveness, 0.58).
narrative_ontology:constraint_metric(dignified_death__sanctity_primary, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(dignified_death__sanctity_primary, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dignified_death__sanctity_primary, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(dignified_death__sanctity_primary, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dignified_death__sanctity_primary, snare).
narrative_ontology:human_readable(dignified_death__sanctity_primary, "Sanctity-of-Life Prohibition on Assisted Death").
narrative_ontology:topic_domain(dignified_death__sanctity_primary, "bioethics/medical_law/political_philosophy").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dignified_death__sanctity_primary, 'b7aee150-e995-4f53-b550-1e4291bc4ffb').
narrative_ontology:cs_kernel_codification('b7aee150-e995-4f53-b550-1e4291bc4ffb', distributed).
narrative_ontology:cs_authority_grounding('b7aee150-e995-4f53-b550-1e4291bc4ffb', lineage).
narrative_ontology:cs_interpretation_layer_present('b7aee150-e995-4f53-b550-1e4291bc4ffb').
narrative_ontology:cs_reading_relation('b7aee150-e995-4f53-b550-1e4291bc4ffb', dignified_death__autonomy_primary, forecloses).
narrative_ontology:cs_reading_relation('b7aee150-e995-4f53-b550-1e4291bc4ffb', dignified_death__relational_autonomy, influences).
narrative_ontology:cs_axiom('b7aee150-e995-4f53-b550-1e4291bc4ffb', foundational, life_possesses_transcendent_intrinsic_value_independent_of_consent).
narrative_ontology:cs_axiom_status(life_possesses_transcendent_intrinsic_value_independent_of_consent, holdable).
narrative_ontology:cs_axiom_grounding('b7aee150-e995-4f53-b550-1e4291bc4ffb', life_possesses_transcendent_intrinsic_value_independent_of_consent, deontological).
narrative_ontology:cs_axiom('b7aee150-e995-4f53-b550-1e4291bc4ffb', foundational, individual_consent_cannot_override_transcendent_moral_law).
narrative_ontology:cs_axiom_status(individual_consent_cannot_override_transcendent_moral_law, holdable).
narrative_ontology:cs_axiom_grounding('b7aee150-e995-4f53-b550-1e4291bc4ffb', individual_consent_cannot_override_transcendent_moral_law, theological).
narrative_ontology:cs_axiom('b7aee150-e995-4f53-b550-1e4291bc4ffb', secondary, absolute_prohibition_is_necessary_means_to_protect_vulnerable_from_coercion).
narrative_ontology:cs_axiom_status(absolute_prohibition_is_necessary_means_to_protect_vulnerable_from_coercion, holdable).
narrative_ontology:cs_axiom_grounding('b7aee150-e995-4f53-b550-1e4291bc4ffb', absolute_prohibition_is_necessary_means_to_protect_vulnerable_from_coercion, instrumental).
narrative_ontology:cs_reference_frame('b7aee150-e995-4f53-b550-1e4291bc4ffb', absolute_prohibition_natural_law_tradition).
narrative_ontology:cs_drift_state('b7aee150-e995-4f53-b550-1e4291bc4ffb', post_oregon_death_with_dignity_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('b7aee150-e995-4f53-b550-1e4291bc4ffb', '').
narrative_ontology:cs_kernel_id(dignified_death__sanctity_primary, dignified_death).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dignified_death__sanctity_primary, religious_institutions).
narrative_ontology:constraint_beneficiary(dignified_death__sanctity_primary, disability_rights_organizations_opposing_legalization).
narrative_ontology:constraint_beneficiary(dignified_death__sanctity_primary, palliative_care_establishment).
narrative_ontology:constraint_beneficiary(dignified_death__sanctity_primary, moral_order_stakeholders).
narrative_ontology:constraint_victim(dignified_death__sanctity_primary, terminally_ill_patients_seeking_death).
narrative_ontology:constraint_victim(dignified_death__sanctity_primary, elderly_dependents_under_family_pressure).
narrative_ontology:constraint_victim(dignified_death__sanctity_primary, disabled_people_denied_autonomy).
narrative_ontology:constraint_victim(dignified_death__sanctity_primary, low_income_patients_without_palliative_access).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(dignified_death__sanctity_primary, medical_professional_bodies).
narrative_ontology:constraint_vindicates(dignified_death__sanctity_primary, sanctity_of_life_doctrine).
narrative_ontology:constraint_vindicates(dignified_death__sanctity_primary, transcendent_moral_law).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Lobbies legislatures and courts to maintain the prohibition on assisted death, framing the prohibition as protection of the vulnerable and adherence to transcendent moral order. Does not itself bear the cost of prolonged suffering; supplies the doctrinal architecture and much of the political organizing capacity that keeps the prohibition in force.
narrative_ontology:constraint_stakeholder(dignified_death__sanctity_primary, religious_institutions, agenda_setter,
    institutional, civilizational, analytical, national).

% Advocates against legalization on the ground that legal assisted death creates social pressure toward death for disabled people whose lives are undervalued by the healthcare system. Genuinely fears coercion but is itself a minority voice within the disabled community, some of whom support access; its position is amplified by the same coalition that benefits from prohibition for unrelated doctrinal reasons.
narrative_ontology:constraint_stakeholder(dignified_death__sanctity_primary, disability_rights_organizations_opposing_legalization, beneficiary,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(dignified_death__sanctity_primary, disability_rights_organizations_opposing_legalization, excluded).

% Positions palliative and hospice care as the sole legitimate response to terminal suffering. Institutional funding, referral pipelines, and professional identity are partly built around being the only sanctioned end-of-life pathway; legalization would introduce a competing pathway and could shift resources or patients away from extended palliative treatment.
narrative_ontology:constraint_stakeholder(dignified_death__sanctity_primary, palliative_care_establishment, beneficiary,
    institutional, biographical, mobile, national).

% Experience unrelieved suffering from terminal illness and want the option to end life on their own timeline. The prohibition forecloses this option entirely regardless of consent, competence, or documented suffering; the only exits are covert self-harm, traveling to a jurisdiction that permits assistance (if resources allow), or continued suffering until natural death.
narrative_ontology:constraint_stakeholder(dignified_death__sanctity_primary, terminally_ill_patients_seeking_death, payer,
    powerless, immediate, trapped, local).

% Live under prohibition's protective framing while simultaneously being cited as its primary justification — the prohibition is defended as shielding them from coercion, but the same powerlessness that makes them vulnerable to coercion toward death also makes them vulnerable to coercion toward unwanted prolongation (family financial strain, caregiver burnout, institutional bed pressure). The prohibition does not resolve this vulnerability; it forecloses one direction of it while leaving the other unaddressed.
narrative_ontology:constraint_stakeholder(dignified_death__sanctity_primary, elderly_dependents_under_family_pressure, payer,
    powerless, immediate, trapped, local).

% Denied the same end-of-life decision authority afforded to non-disabled people making comparable medical choices, on grounds that their consent cannot be trusted to be free of internalized ableism or external pressure. Experience the prohibition as a paternalistic denial of the autonomy claimed for every other medical decision in their lives.
narrative_ontology:constraint_stakeholder(dignified_death__sanctity_primary, disabled_people_denied_autonomy, payer,
    powerless, biographical, trapped, national).

% Lack access to the high-quality palliative care that the prohibition's defenders point to as the humane alternative. For this population, the prohibition does not substitute a genuine alternative — it simply forecloses the option while the promised alternative (adequate palliative care) remains unfunded and unavailable in practice.
narrative_ontology:constraint_stakeholder(dignified_death__sanctity_primary, low_income_patients_without_palliative_access, payer,
    powerless, immediate, trapped, local).

% Write and adjudicate the statutes that criminalize or permit assisted death. Respond to lobbying pressure from religious and disability-rights coalitions on one side and patient autonomy and civil liberties advocates on the other; hold the formal power to change the constraint but face sustained organized resistance to doing so.
narrative_ontology:constraint_stakeholder(dignified_death__sanctity_primary, legislatures_and_courts, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(dignified_death__sanctity_primary, legislatures_and_courts, observer).

% Navigate professional ethics codes shaped by the sanctity framework; some physicians support legalization as a matter of patient-centered care, others rely on the prohibition to avoid participating in a practice they find morally objectionable. Their professional liability and licensing exposure is directly shaped by which reading of dignity prevails in law.
narrative_ontology:constraint_stakeholder(dignified_death__sanctity_primary, medical_professional_bodies, observer,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(dignified_death__sanctity_primary, medical_professional_bodies, beneficiary).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared, bright-line moral and legal standard that protects against coercion, undervaluation of disabled and elderly lives, and slippery-slope expansion of who may be deemed appropriately dead — a genuine collective-action concern given documented cases of financial and caregiver pressure in permissive jurisdictions.
% TRANSFER_FUNCTION: Moves decision authority over the timing and manner of death away from the individual patient and toward institutional and doctrinal authorities (religious bodies, legislatures, the palliative care establishment), while moving the cost of that transfer — prolonged suffering, loss of autonomy, foreclosed exit — onto patients who would otherwise choose to end their lives, and their families.
% ABSENT_VOICES: Terminally ill patients who die during the years-long legislative and judicial process are permanently absent from the debate that determines their fate; disabled people who support legalized access are frequently talked over by disability-rights organizations that oppose it in their name; low-income patients without palliative access rarely testify before the legislative committees whose members disproportionately hear from well-resourced advocacy coalitions on both sides.
% DISAPPEARANCE_RATIONALE: Sanctity-primary advocates hold the world would rearrange catastrophically — coercion of vulnerable populations would proliferate, and the moral valuation of disabled and elderly lives would erode. Patient-autonomy advocates hold the world would rearrange in the opposite direction — unrelieved suffering would end and autonomy would be restored, with jurisdictions that have already adopted alternative readings (Oregon, Netherlands, Canada) serving as the natural experiment. Both readings agree the world rearranges; they dispute the direction and magnitude, which is why this is authored contested rather than world_rearranges outright.
% FOUNDING_PROBLEM: Historically: prevent extrajudicial killing of the terminally ill, the disabled, and social undesirables under cover of 'mercy,' as practiced in eugenic and totalitarian regimes; establish an unconditional floor under the value of human life that resists utilitarian calculation by the powerful about whose life is worth continuing.
% FOUNDING_PROBLEM_CORROBORATION: Historians of eugenics and eugenic-adjacent euthanasia programs (independent of any religious institution) corroborate that the founding problem — coerced or state-directed killing of the vulnerable dressed as mercy — was real and remains a live concern in jurisdictions with weak safeguards. Independent disability-studies researchers outside the beneficiary coalition corroborate ongoing risk of subtle coercion in permissive regimes. However, palliative-care outcomes researchers and comparative-law scholars studying operating safeguard regimes (Oregon's 25+ year record, Belgium's reporting data) attest that the specific coercion risk the prohibition targets can be substantially mitigated by procedural safeguards short of blanket prohibition — suggesting the founding problem, while real, may no longer require this particular solution in its current absolute form.
narrative_ontology:disappearance_verdict(dignified_death__sanctity_primary, contested).
narrative_ontology:founding_problem_status(dignified_death__sanctity_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dignified_death__sanctity_primary, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(dignified_death__sanctity_primary, 'none', 1).
narrative_ontology:epsilon_provenance(dignified_death__sanctity_primary, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dignified_death__sanctity_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(dignified_death__sanctity_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(dignified_death__sanctity_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.58, within the expected 0.50-0.65 range for this reading's legalization-blocking effect: the prohibition extracts autonomy and imposes suffering-continuation costs on a clearly identifiable set of powerless, trapped victims, while the benefit (protection from coercion, moral order preservation) accrues to institutional actors who do not themselves bear the cost of the foreclosure. Suppression is authored high (0.71) and rising over the interval because maintaining an absolute prohibition against a competing, increasingly legally successful reading requires escalating active defense — legislative lobbying, litigation, professional ethics enforcement — rather than passive background acceptance. Theater ratio is moderate and rising (0.42) because some of the prohibition's defense increasingly consists of invoking vulnerable populations (elderly, disabled, poor) as justification without those populations' own preferences being centered or their alternative vulnerabilities (unwanted prolongation, inadequate palliative access) being addressed with comparable resources. Accessibility collapse is moderate (0.48), reflecting that alternatives are NOT fully suppressed — patients can travel to permissive jurisdictions if resources allow, and covert self-managed exits exist, though both carry severe costs. Resistance is substantial (0.62), reflecting organized patient-autonomy and disability-autonomy advocacy actively contesting the prohibition in courts and legislatures.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat (religious institutions, legislatures defending the status quo), the constraint reads as principled moral coordination preventing a slide toward devaluing vulnerable lives. From the payer seats (terminally ill patients, disabled people denied autonomy), the identical structure reads as coercive prolongation of suffering under a moral framework they did not choose and cannot exit. The engine computes both readings from the same structural data — the divergence is not a modeling error but the object of study.
 *
 * DIRECTIONALITY LOGIC:
 *   Religious institutions and the palliative care establishment sit near the beneficiary end of directionality: they set the terms, bear none of the direct suffering cost, and derive institutional legitimacy or resource flow from the prohibition's persistence. Terminally ill patients, elderly dependents, disabled people, and low-income patients sit near the full-target end: they are powerless, trapped (no meaningful exit for most), and bear the constraint's cost directly and immediately. Disability rights organizations opposing legalization are authored as a genuine coordination beneficiary rather than a captured proxy — their coercion concern is real and independently documented — but they are also partly instrumentalized by a broader coalition whose interest in prohibition is doctrinal rather than protective, which is why they carry a secondary excluded role: their internal dissenters (disabled people who want access) are not amplified by the same coalition.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — preventing coerced or eugenic killing of the vulnerable — was real and remains partially live, which is precisely why this constraint is not simply relabeled as a pure snare with no coordination function; genuine protective coordination exists. But the R5 corroboration shows the specific mechanism (absolute prohibition, rather than procedural safeguards) is contested by independent comparative-law and palliative-outcomes researchers who find the coercion risk manageable through regulated, consent-verified frameworks operating in other jurisdictions. This is the mandatrophy signature: an arrangement whose founding problem was real is being defended in its original absolute form after less costly solutions (procedural safeguards) have been demonstrated elsewhere, and the defense is increasingly maintained through invoking the vulnerable rather than centering their actual stated preferences. The classification as snare captures that the coordination story (protecting the vulnerable) has become cover for continued extraction (imposed suffering on those the coordination story claims to protect) once alternative jurisdictions demonstrate the coordination function is separable from absolute prohibition.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    protective_function_vs_coercive_residue,
    'Does the absolute prohibition still perform a necessary protective function against coercion of vulnerable populations, or has that function become separable from absolute prohibition given demonstrated procedural safeguards in permissive jurisdictions?',
    'Longitudinal comparative analysis of coercion incidence, safeguard-violation rates, and vulnerable-population outcomes in jurisdictions with regulated assisted-death frameworks (Oregon, Netherlands, Belgium, Canada) versus jurisdictions maintaining absolute prohibition, controlling for palliative care access quality.',
    'If procedural safeguards demonstrably manage the coercion risk at rates comparable to or better than absolute prohibition, the sanctity-primary reading''s coordination claim weakens substantially and the classification moves further toward pure snare; if safeguard regimes show measurable coercion drift over time, the sanctity-primary reading''s protective claim gains empirical support and the classification moves toward tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(protective_function_vs_coercive_residue, empirical, 'Whether absolute prohibition remains necessary or whether its protective function is separable and achievable through lesser means.').

omega_variable(
    transcendent_moral_law_naturalness,
    'Is the claim that intentional life-termination violates transcendent moral law a discoverable natural-law fact independent of human construction, or a constructed doctrinal claim that benefits specific institutional actors (religious institutions, the palliative care establishment) who derive authority, resources, or legitimacy from its enforcement?',
    'This question is not resolvable by empirical inquiry in the way the coercion question is — it depends on metaethical and theological commitments that are themselves contested. What can be examined empirically is whether the specific institutional actors who most vigorously defend the doctrine also derive concentrated material or organizational benefit from its enforcement, which would be consistent with (though not dispositive of) a constructed-doctrine reading.',
    'If treated as genuine transcendent natural law, the constraint''s beneficiary declarations become an artifact of who happens to correctly perceive the natural law, and the classification pressure toward snare weakens considerably. If treated as constructed doctrine serving identifiable institutional interests, the beneficiary structure is causally central to the classification and the snare reading is reinforced.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(transcendent_moral_law_naturalness, conceptual, 'Whether the sanctity claim is discovered natural law or constructed doctrine serving institutional beneficiaries — the central metaethical uncertainty underlying this reading''s classification.').

omega_variable(
    disability_advocacy_authenticity_vs_instrumentalization,
    'To what extent do disability rights organizations opposing legalization represent the authentic, majority preference of disabled people, versus a vocal minority whose concerns are amplified because they align with a broader coalition''s doctrinal interest in prohibition?',
    'Representative survey research directly polling disabled people (not disability advocacy organizations) on assisted-death legalization preferences, compared against the public positions taken by major disability rights organizations in legislative testimony.',
    'If survey data shows disabled people''s preferences are more divided or more supportive of legalization than organizational advocacy suggests, this strengthens the reading that disability rights opposition is partially instrumentalized within the broader sanctity coalition; if survey data aligns closely with organizational positions, the coordination-beneficiary framing for this stakeholder group is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(disability_advocacy_authenticity_vs_instrumentalization, empirical, 'Whether disability rights opposition to legalization authentically represents disabled people''s preferences or is amplified beyond its representative weight.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dignified_death__sanctity_primary, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dign_tr_t0, dignified_death__sanctity_primary, theater_ratio, 0, 0.22).
narrative_ontology:measurement(dign_tr_t8, dignified_death__sanctity_primary, theater_ratio, 8, 0.27).
narrative_ontology:measurement(dign_tr_t16, dignified_death__sanctity_primary, theater_ratio, 16, 0.31).
narrative_ontology:measurement(dign_tr_t24, dignified_death__sanctity_primary, theater_ratio, 24, 0.35).
narrative_ontology:measurement(dign_tr_t32, dignified_death__sanctity_primary, theater_ratio, 32, 0.39).
narrative_ontology:measurement(dign_tr_t40, dignified_death__sanctity_primary, theater_ratio, 40, 0.42).

% Extraction over time
narrative_ontology:measurement(dign_be_t0, dignified_death__sanctity_primary, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(dign_be_t8, dignified_death__sanctity_primary, base_extractiveness, 8, 0.42).
narrative_ontology:measurement(dign_be_t16, dignified_death__sanctity_primary, base_extractiveness, 16, 0.47).
narrative_ontology:measurement(dign_be_t24, dignified_death__sanctity_primary, base_extractiveness, 24, 0.51).
narrative_ontology:measurement(dign_be_t32, dignified_death__sanctity_primary, base_extractiveness, 32, 0.55).
narrative_ontology:measurement(dign_be_t40, dignified_death__sanctity_primary, base_extractiveness, 40, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(dign_su_t0, dignified_death__sanctity_primary, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(dign_su_t8, dignified_death__sanctity_primary, suppression_requirement, 8, 0.6).
narrative_ontology:measurement(dign_su_t16, dignified_death__sanctity_primary, suppression_requirement, 16, 0.63).
narrative_ontology:measurement(dign_su_t24, dignified_death__sanctity_primary, suppression_requirement, 24, 0.66).
narrative_ontology:measurement(dign_su_t32, dignified_death__sanctity_primary, suppression_requirement, 32, 0.69).
narrative_ontology:measurement(dign_su_t40, dignified_death__sanctity_primary, suppression_requirement, 40, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dignified_death__sanctity_primary, identity_coordination).
narrative_ontology:boltzmann_floor_override(dignified_death__sanctity_primary, 0.08).
narrative_ontology:affects_constraint(dignified_death__sanctity_primary, dignified_death__autonomy_primary).
narrative_ontology:affects_constraint(dignified_death__sanctity_primary, dignified_death__relational_autonomy).

% DUAL FORMULATION NOTE:
% This constraint is one of three linked readings of the dignified_death kernel. dignified_death__sanctity_primary (this file) authors the reading in which dignity resides in life's intrinsic transcendent value and intentional termination is impermissible regardless of consent — classified here as approaching snare, with victims among vulnerable populations denied autonomy and beneficiaries among institutional actors (religious bodies, palliative care establishment) who do not bear the cost of foreclosure. dignified_death__autonomy_primary authors the reading in which dignity resides in self-determination and the suffering individual holds final decision authority — expected to classify closer to rope or tangled_rope depending on safeguard structure, with a substantially different (likely near-absent) victim set. dignified_death__relational_autonomy authors the reading in which decision authority is distributed across a patient-family-clinician triad with procedural safeguards — expected to classify as tangled_rope or scaffold, balancing coordination function against residual extraction risk from family or institutional pressure within the triad. All three readings share the same underlying kernel (where does dignity reside, who holds decision authority over death) but instantiate structurally distinct constraints with different ε, different beneficiary/victim sets, and different classifications. This reading's high suppression and rising theater_ratio reflect that it is the reading increasingly on the defensive as the other two readings gain legal ground in comparative jurisdictions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
