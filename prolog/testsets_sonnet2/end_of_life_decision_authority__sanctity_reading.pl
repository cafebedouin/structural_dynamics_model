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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
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
 *   human_readable: Sanctity-of-Life Prohibition on Intentional Life-Ending (Sanctity Reading of End-of-Life Authority)
 *   domain: medical_ethics/bioethics/end_of_life_policy
 *
 * SUMMARY:
 *   This story instantiates the sanctity reading of the contested end-of-life
 *   decision authority kernel: the claim that human life possesses intrinsic
 *   value independent of the will of the person living it, such that
 *   intentional life-ending — whether by the individual's own request or a
 *   physician's hand — violates that value regardless of consent or
 *   suffering. As a standalone constraint this reading operates as a
 *   categorical prohibition enforced through medical licensing law and
 *   criminal statute. It genuinely coordinates a real social good (preventing
 *   coerced or eugenic life-ending of the vulnerable) while also imposing
 *   extraction on a specific population: patients whose settled, uncoerced
 *   judgment about their own remaining life is overridden by the rule with no
 *   case-by-case exception mechanism. The prohibition's own metrics
 *   (extraction, suppression) are authored for the standing sanctity-grounded
 *   prohibition as it currently operates, not for any alternative regime this
 *   reading would prefer to install.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(end_of_life_decision_authority__sanctity_reading, 0.58).
domain_priors:suppression_score(end_of_life_decision_authority__sanctity_reading, 0.62).
domain_priors:theater_ratio(end_of_life_decision_authority__sanctity_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(end_of_life_decision_authority__sanctity_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(end_of_life_decision_authority__sanctity_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(end_of_life_decision_authority__sanctity_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(end_of_life_decision_authority__sanctity_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(end_of_life_decision_authority__sanctity_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(end_of_life_decision_authority__sanctity_reading, tangled_rope).
narrative_ontology:human_readable(end_of_life_decision_authority__sanctity_reading, "Sanctity-of-Life Prohibition on Intentional Life-Ending (Sanctity Reading of End-of-Life Authority)").
narrative_ontology:topic_domain(end_of_life_decision_authority__sanctity_reading, "medical_ethics/bioethics/end_of_life_policy").

domain_priors:requires_active_enforcement(end_of_life_decision_authority__sanctity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(end_of_life_decision_authority__sanctity_reading, 'f5049a9c-75b1-4942-852e-8f2e39591c68').
narrative_ontology:cs_kernel_codification('f5049a9c-75b1-4942-852e-8f2e39591c68', distributed).
narrative_ontology:cs_authority_grounding('f5049a9c-75b1-4942-852e-8f2e39591c68', lineage).
narrative_ontology:cs_interpretation_layer_present('f5049a9c-75b1-4942-852e-8f2e39591c68').
narrative_ontology:cs_reading_relation('f5049a9c-75b1-4942-852e-8f2e39591c68', end_of_life_decision_authority__autonomy_reading, forecloses).
narrative_ontology:cs_reading_relation('f5049a9c-75b1-4942-852e-8f2e39591c68', end_of_life_decision_authority__vulnerability_protection_reading, influences).
narrative_ontology:cs_axiom('f5049a9c-75b1-4942-852e-8f2e39591c68', foundational, life_value_independent_of_individual_will).
narrative_ontology:cs_axiom_status(life_value_independent_of_individual_will, holdable).
narrative_ontology:cs_axiom_grounding('f5049a9c-75b1-4942-852e-8f2e39591c68', life_value_independent_of_individual_will, deontological).
narrative_ontology:cs_axiom('f5049a9c-75b1-4942-852e-8f2e39591c68', foundational, physician_role_confined_to_healing).
narrative_ontology:cs_axiom_status(physician_role_confined_to_healing, holdable).
narrative_ontology:cs_axiom_grounding('f5049a9c-75b1-4942-852e-8f2e39591c68', physician_role_confined_to_healing, conventional).
narrative_ontology:cs_reference_frame('f5049a9c-75b1-4942-852e-8f2e39591c68', categorical_prohibition_on_intentional_life_ending).
narrative_ontology:cs_drift_state('f5049a9c-75b1-4942-852e-8f2e39591c68', post_legalization_wave_jurisdictions, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('f5049a9c-75b1-4942-852e-8f2e39591c68', '').
narrative_ontology:cs_kernel_id(end_of_life_decision_authority__sanctity_reading, end_of_life_decision_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(end_of_life_decision_authority__sanctity_reading, disability_rights_advocates).
narrative_ontology:constraint_beneficiary(end_of_life_decision_authority__sanctity_reading, palliative_care_institutions).
narrative_ontology:constraint_beneficiary(end_of_life_decision_authority__sanctity_reading, religious_healthcare_systems).
narrative_ontology:constraint_victim(end_of_life_decision_authority__sanctity_reading, terminally_ill_patients_seeking_death).
narrative_ontology:constraint_victim(end_of_life_decision_authority__sanctity_reading, chronic_intractable_suffering_patients).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(end_of_life_decision_authority__sanctity_reading, treating_physicians).
narrative_ontology:constraint_vindicates(end_of_life_decision_authority__sanctity_reading, intrinsic_human_dignity_doctrine).
narrative_ontology:constraint_vindicates(end_of_life_decision_authority__sanctity_reading, physician_healer_role_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Write and enforce statutes and professional codes that criminalize or professionally sanction physicians who intentionally hasten death, grounding the prohibition in the claim that life has value independent of the sufferer's own assessment. They administer licensing consequences and criminal referral for violation, and can revise the prohibition through legislative or regulatory process.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__sanctity_reading, medical_boards_and_legislatures, agenda_setter,
    institutional, generational, analytical, national).

% Operate hospitals and hospice networks whose institutional identity and funding models are built on a healer-only, life-affirming mandate. The prohibition vindicates their doctrinal position and protects their institutional monopoly over end-of-life care against a rival model (assisted death) that would require them either to participate against conscience or cede market share to secular providers.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__sanctity_reading, religious_healthcare_systems, beneficiary,
    organized, civilizational, arbitrage, national).

% Support the prohibition because they see themselves as the population most exposed if intentional life-ending becomes available — disabled and chronically ill people whose lives are already systematically undervalued by cost-conscious institutions. The blanket rule protects them by refusing to let anyone, including the disabled person themselves, certify a life as not worth continuing.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__sanctity_reading, disability_rights_advocates, beneficiary,
    organized, generational, constrained, national).

% Experience prolonged, medically unrelievable suffering at the end of life and want a physician-assisted or physician-administered death, which the prohibition denies them regardless of their own settled judgment. Their only formal exits are enduring the illness to natural death, seeking unregulated or clandestine means, or traveling to a jurisdiction with different rules — options foreclosed by poverty, immobility, or the terminal condition itself.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__sanctity_reading, terminally_ill_patients_seeking_death, payer,
    powerless, immediate, trapped, local).

% Live with severe, non-terminal but intractable suffering (chronic pain, progressive neurological disease) for years or decades under a rule that categorically denies any intentional death regardless of duration or severity of suffering, because the rule's justification (intrinsic value independent of will) does not admit degrees or exceptions tied to prognosis.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__sanctity_reading, chronic_intractable_suffering_patients, payer,
    powerless, biographical, trapped, local).

% Are professionally bound to a healer-only role and personally exposed to license loss or prosecution if they act on a patient's request to hasten death, even when they judge the request medically and ethically reasonable. They administer the prohibition at the bedside and bear the emotional cost of enforcing it against patients they cannot help in the way requested.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__sanctity_reading, treating_physicians, agenda_setter,
    powerful, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(end_of_life_decision_authority__sanctity_reading, treating_physicians, payer).

% Witness prolonged suffering and often share the patient's wish for a hastened death, but have no standing in the prohibition's framework — the rule is authored around the patient's will versus an external value claim, and family testimony about suffering is not treated as evidence that could modify the rule.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__sanctity_reading, family_members_of_dying_patients, excluded,
    moderate, biographical, constrained, local).

% Adjudicate constitutional and ethical challenges to the prohibition, weighing the sanctity claim against competing autonomy and vulnerability-protection framings, and can shift the prohibition's scope through case law or academic consensus without holding the underlying value question settled.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__sanctity_reading, bioethicists_and_courts, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(end_of_life_decision_authority__sanctity_reading, diffuse).
narrative_ontology:fixing_cost_class(end_of_life_decision_authority__sanctity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, non-negotiable bright line — no intentional life-ending, ever, regardless of consent — that removes the need for case-by-case adjudication of whether a given instance of hastened death is justified, and forecloses the slippery-slope risk of expanding eligibility criteria once any exception is granted.
% TRANSFER_FUNCTION: Moves the authority to end suffering away from the individual sufferer and the treating physician and vests it in institutional/legal actors who hold the line; the cost of that transfer is borne entirely by patients whose own settled judgment about their remaining life is overridden, while the benefit accrues to institutions and advocacy communities whose position and funding depend on the healer-only, no-exceptions framing holding.
% ABSENT_VOICES: Family members who witness the suffering directly are not evidentiary parties to the rule. Patients who have already died un-relieved cannot testify to what the rule cost them. Physicians who privately believe the rule is wrong in specific cases are professionally silenced from acting on that belief even where they could otherwise document it.
% DISAPPEARANCE_RATIONALE: If the sanctity reading's prohibition vanished overnight, jurisdictions would default to whichever sibling framework filled the vacuum (autonomy-based legalization or vulnerability-protection checkpoints) — religious healthcare systems and disability advocates would lose the categorical backstop they rely on, while currently trapped patients would gain an exit. Whether the world 'rearranges' or 'stays the same' depends entirely on which sibling reading is asked; sanctity-reading proponents would say the moral fact of intrinsic value doesn't change even if the law does, while payers would say their situation changes completely.
% FOUNDING_PROBLEM: Historically built to prevent physicians and institutions from treating any life — especially disabled, elderly, or socially devalued lives — as terminable by external judgment, in the aftermath of eugenic and euthanasia abuses where 'quality of life' assessments were used to justify killing people who had not requested it.
% FOUNDING_PROBLEM_CORROBORATION: Disability rights organizations (a constituency that is not the primary economic beneficiary of the rule) independently corroborate that the founding problem — non-consensual life-ending disguised as mercy — remains live in jurisdictions with expanding euthanasia eligibility criteria (e.g., documented eligibility creep in some legalized regimes). Palliative-care and religious institutions, who are direct beneficiaries, also assert this but cannot serve as independent corroboration. Patients experiencing intractable suffering, and courts in jurisdictions that have legalized assisted death without documented abuse, dispute that the founding problem is still live in a form that justifies a categorical rule rather than a regulated exception.
narrative_ontology:disappearance_verdict(end_of_life_decision_authority__sanctity_reading, contested).
narrative_ontology:founding_problem_status(end_of_life_decision_authority__sanctity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(end_of_life_decision_authority__sanctity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(end_of_life_decision_authority__sanctity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(end_of_life_decision_authority__sanctity_reading, 0.58, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is authored at a moderate-high 0.58 because the prohibition transfers real decisional authority away from a specific, identifiable population (terminally and chronically suffering patients) with no exit valve within the rule's own terms; it is not higher because the coordination function (protecting the non-requesting vulnerable from coerced death) is genuine and substantial, not merely a cover story. Suppression is authored at 0.62 because the rule is actively enforced through licensing and criminal law against physicians and, functionally, against patients who cannot access the outcome they seek through any legal channel. Accessibility collapse is moderate (0.4) rather than high because informal and cross-border alternatives (unregulated methods, medical tourism) persist, unlike a genuine mountain where no alternative exists at all. Resistance is high (0.72) because this reading is actively contested by courts, legislatures moving toward the sibling readings, and patient advocacy — this is a live, defended position, not settled fact.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setting seat (medical boards, legislatures, religious institutions) the prohibition is experienced as principled coordination protecting the vulnerable — a genuine rope. From the payer seat (patients in intractable suffering, trapped by the rule's categorical structure) the same rule computes as extraction: their own settled judgment is overridden by a value claim they may not share, enforced without recourse. The engine should compute divergent seat classifications from these structural facts rather than from the story's single claimed_type label.
 *
 * DIRECTIONALITY LOGIC:
 *   Religious healthcare systems and disability rights advocates are coded as beneficiaries because the prohibition's persistence directly protects their institutional position and the population they represent from a feared downstream harm (non-consensual life-ending disguised as mercy) — even though disability advocates are not economically extracting, their organizational standing and moral argument depend on the rule holding, which the derivation reads as low d. Terminally ill and chronically suffering patients are the highest-d seats: trapped exit options, immediate/biographical time horizons, and a rule whose entire justification (intrinsic value independent of will) explicitly overrides their own stated preference. Treating physicians occupy a dual seat — they administer the prohibition (agenda_setter) but also personally bear its constraint on their professional judgment (payer), which the secondary_role captures without needing an override.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — preventing devalued lives from being ended without genuine consent, often under institutional or family pressure — remains partially live (documented eligibility creep in some legalized jurisdictions corroborates this from outside the direct beneficiary set), which is why founding_problem_status is authored as contested rather than dead. This prevents the story from being mislabeled as pure extraction: the sanctity reading is not merely a snare wearing a moral costume, because a real, externally-corroborated vulnerable population exists whose protection the rule serves. But the rule's blanket, no-exception structure also generates a second, disjoint victim class (competent, uncoerced sufferers) whose harm the rule's own framework cannot recognize as harm, because the framework defines the value at stake as independent of any individual's will — which is why tangled_rope rather than rope or mountain is the structurally accurate claim.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sanctity_kernel_reading_disagreement_locus,
    'The three readings of the end_of_life_decision_authority kernel disagree about where moral authority over death properly sits: with the individual (autonomy_reading), with distributed institutional process (vulnerability_protection_reading), or with a value external to any will (sanctity_reading, this story). Which locus is correct is not resolvable by the metrics authored here.',
    'Not empirically resolvable in the ordinary sense — this is a first-order normative disagreement about the source of moral authority over death. What IS empirically tractable is the downstream question of whether legalized alternatives in practice produce documented coercion of the non-requesting vulnerable (which would support sanctity/vulnerability readings) or documented denial of relief to competent sufferers (which would support autonomy/vulnerability readings). Neither resolves the foundational premise.',
    'If eligibility-creep evidence accumulates showing non-consensual pressure under legalized regimes, this reading''s beneficiary claim (disability rights protection) strengthens and its extractiveness score would be defensible as lower than authored; if instead legalized regimes show no such creep over decades, the victim-side extraction (trapped competent sufferers) becomes the dominant empirical fact and this reading''s ε would need to be authored higher, not lower.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sanctity_kernel_reading_disagreement_locus, conceptual, 'Which of the three kernel readings locates moral authority over death correctly is a conceptual disagreement, not an empirical one; only downstream, contingent facts about implementation are empirically tractable.').

omega_variable(
    sibling_reading_structural_delta,
    'The autonomy_reading and vulnerability_protection_reading siblings author different victim sets and different physician roles for structurally the same underlying kernel — autonomy_reading treats physicians as agents of patient will rather than healers-only, and its victim set (patients denied assistance) is disjoint from this reading''s victim set (patients granted death under pressure). Are these genuinely three different constraints, or one constraint viewed through three lenses?',
    'Per the ε-invariance principle, since each reading assigns a stably different ε, victim set, and physician-role structure to what is nominally the same real-world policy question, they are authored as three separate constraint stories linked via cs_structure.reading_relations rather than as one story with a measurement parameter.',
    'Confirms the decomposition choice already made: this file is deliberately narrow (sanctity_reading only) rather than attempting to average or hedge across the three readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_structural_delta, conceptual, 'Documents why the kernel was decomposed into three linked stories rather than authored as one constraint with an observable parameter.').

omega_variable(
    eligibility_creep_evidentiary_status,
    'Disability rights advocates cite documented eligibility creep in some legalized-euthanasia jurisdictions as corroboration that the founding problem remains live; opponents dispute the causal reading of that same data (attributing it to expanded medical indications rather than coercive pressure). Is the eligibility-creep evidence itself contested on interpretation, not just on policy conclusion?',
    'Independent longitudinal case-review studies distinguishing expanded-indication cases from cases involving documented external pressure or inadequate capacity assessment, ideally conducted by researchers with no institutional stake in either the sanctity or autonomy framings.',
    'If eligibility-creep cases are predominantly indication-expansion rather than coercion, the sanctity reading''s core empirical corroboration weakens substantially, which would push its authored extractiveness upward (less genuine coordination function, more pure extraction from trapped sufferers) and could shift its structural type from tangled_rope toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(eligibility_creep_evidentiary_status, empirical, 'Whether the corroborating evidence for the founding problem''s continued liveness is itself sound or contested on interpretation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(end_of_life_decision_authority__sanctity_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(end__tr_t0, end_of_life_decision_authority__sanctity_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(end__tr_t8, end_of_life_decision_authority__sanctity_reading, theater_ratio, 8, 0.2).
narrative_ontology:measurement(end__tr_t16, end_of_life_decision_authority__sanctity_reading, theater_ratio, 16, 0.22).
narrative_ontology:measurement(end__tr_t24, end_of_life_decision_authority__sanctity_reading, theater_ratio, 24, 0.24).
narrative_ontology:measurement(end__tr_t32, end_of_life_decision_authority__sanctity_reading, theater_ratio, 32, 0.26).
narrative_ontology:measurement(end__tr_t40, end_of_life_decision_authority__sanctity_reading, theater_ratio, 40, 0.28).

% Extraction over time
narrative_ontology:measurement(end__be_t0, end_of_life_decision_authority__sanctity_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(end__be_t8, end_of_life_decision_authority__sanctity_reading, base_extractiveness, 8, 0.48).
narrative_ontology:measurement(end__be_t16, end_of_life_decision_authority__sanctity_reading, base_extractiveness, 16, 0.52).
narrative_ontology:measurement(end__be_t24, end_of_life_decision_authority__sanctity_reading, base_extractiveness, 24, 0.55).
narrative_ontology:measurement(end__be_t32, end_of_life_decision_authority__sanctity_reading, base_extractiveness, 32, 0.57).
narrative_ontology:measurement(end__be_t40, end_of_life_decision_authority__sanctity_reading, base_extractiveness, 40, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(end__su_t0, end_of_life_decision_authority__sanctity_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(end__su_t8, end_of_life_decision_authority__sanctity_reading, suppression_requirement, 8, 0.54).
narrative_ontology:measurement(end__su_t16, end_of_life_decision_authority__sanctity_reading, suppression_requirement, 16, 0.57).
narrative_ontology:measurement(end__su_t24, end_of_life_decision_authority__sanctity_reading, suppression_requirement, 24, 0.6).
narrative_ontology:measurement(end__su_t32, end_of_life_decision_authority__sanctity_reading, suppression_requirement, 32, 0.61).
narrative_ontology:measurement(end__su_t40, end_of_life_decision_authority__sanctity_reading, suppression_requirement, 40, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(end_of_life_decision_authority__sanctity_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(end_of_life_decision_authority__sanctity_reading, end_of_life_decision_authority__autonomy_reading).
narrative_ontology:affects_constraint(end_of_life_decision_authority__sanctity_reading, end_of_life_decision_authority__vulnerability_protection_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the end_of_life_decision_authority kernel. autonomy_reading authors near-zero ε for the same standing prohibition (seeing it as pure denial of sovereign right over one's own death, with physicians as agents of patient will); vulnerability_protection_reading authors a moderate ε with a narrower, more contingent victim set (seeing most sanctity-style harms as mitigable through checkpoint design rather than inherent to any prohibition). sanctity_reading (this file) authors the highest ε and broadest victim set because its foundational premise — intrinsic value independent of will — admits no case-by-case exception, categorically extending the victim set to competent as well as pressured sufferers. Each file's claimed_type, metrics, and stakeholder set are authored independently per the ε-invariance principle; they are linked here rather than merged.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
