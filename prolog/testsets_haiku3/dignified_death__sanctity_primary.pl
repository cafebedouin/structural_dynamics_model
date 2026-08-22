% ============================================================================
% CONSTRAINT STORY: dignified_death__sanctity_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:measurement_basis/2,
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
 *   human_readable: Sanctity of Life Constraint: Transcendent Prohibition on Intentional Life Termination
 *   domain: bioethics/medical_law/political_philosophy
 *
 * SUMMARY:
 *   This constraint instantiates the sanctity-primary reading of the
 *   contested dignified_death kernel. The reading claims that dignity resides
 *   in life's intrinsic, transcendent value—that intentional termination of
 *   human life violates a moral order that transcends individual consent or
 *   suffering. The constraint operates by establishing categorical
 *   prohibition on aid-in-dying and by enforcing mandatory continuation of
 *   life-sustaining treatment regardless of patient preference. The
 *   constraint benefits the moral order / institutional community that
 *   vindicates sanctity doctrine; it extracts from terminally ill patients in
 *   unbearable suffering, elderly people with advance directives, and
 *   severely disabled populations whose preferred end-of-life choices are
 *   foreclosed. The reading is contested by two sibling readings:
 *   autonomy_primary (dignity = self-determination; patient has final
 *   authority) and relational_autonomy (dignity emerges from
 *   family-clinician-patient process with safeguards). The three readings
 *   cannot be authored as a single ε-invariant story: they carry materially
 *   different victim sets, beneficiary structures, and type classifications.
 *   This story generates ONLY the sanctity_primary reading; sibling readings
 *   are separate constraints linked through network.affects_constraints and
 *   cs_structure.reading_relations.
 *
 * KEY AGENTS:
 *   - Moral order / institutional community: agenda-setter and beneficiary; maintains sanctity doctrine through legislative prohibition, medical licensure, and doctrinal teaching
 *   - Terminally ill patients in unbearable suffering: victims; trapped, powerless, immediate time horizon; extraction through mandatory life-prolongation against their stated values
 *   - Elderly with advance directives: victims; identity-locked through accumulated social messaging; moderate power but constrained exit
 *   - Severely disabled populations: victims; powerless, subject to paternalistic overrides; constrained exit amplified by sanctity presumptions
 *   - Clinical providers: payers and enforcers; constrained by licensing boards and criminal law; experience suppression through peer enforcement
 *   - Family members: structurally excluded from decision authority; experience moral distress; excluded by sanctity framing (would have voice in relational-autonomy reading)
 *   - Jurisdictions legalizing aid-in-dying: excluded by sanctity regime; represent feasible institutional alternative that proves constraint is not natural law
 *   - Conservative bioethicists: agenda-setters; produce and propagate intellectual authority for sanctity reading
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
narrative_ontology:constraint_metric(dignified_death__sanctity_primary, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(dignified_death__sanctity_primary, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dignified_death__sanctity_primary, snare).
narrative_ontology:human_readable(dignified_death__sanctity_primary, "Sanctity of Life Constraint: Transcendent Prohibition on Intentional Life Termination").
narrative_ontology:topic_domain(dignified_death__sanctity_primary, "bioethics/medical_law/political_philosophy").

domain_priors:requires_active_enforcement(dignified_death__sanctity_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dignified_death__sanctity_primary, 'd6ca8d8d-4270-464a-8c22-cfd6312adae3').
narrative_ontology:cs_kernel_codification('d6ca8d8d-4270-464a-8c22-cfd6312adae3', fixed_text).
narrative_ontology:cs_authority_grounding('d6ca8d8d-4270-464a-8c22-cfd6312adae3', lineage).
narrative_ontology:cs_interpretation_layer_present('d6ca8d8d-4270-464a-8c22-cfd6312adae3').
narrative_ontology:cs_reading_relation('d6ca8d8d-4270-464a-8c22-cfd6312adae3', dignified_death__autonomy_primary, forecloses).
narrative_ontology:cs_reading_relation('d6ca8d8d-4270-464a-8c22-cfd6312adae3', dignified_death__relational_autonomy, influences).
narrative_ontology:cs_axiom('d6ca8d8d-4270-464a-8c22-cfd6312adae3', foundational, life_intrinsic_transcendent_value).
narrative_ontology:cs_axiom_status(life_intrinsic_transcendent_value, holdable).
narrative_ontology:cs_axiom_grounding('d6ca8d8d-4270-464a-8c22-cfd6312adae3', life_intrinsic_transcendent_value, deontological).
narrative_ontology:cs_axiom('d6ca8d8d-4270-464a-8c22-cfd6312adae3', foundational, intentional_termination_categorically_impermissible).
narrative_ontology:cs_axiom_status(intentional_termination_categorically_impermissible, holdable).
narrative_ontology:cs_axiom_grounding('d6ca8d8d-4270-464a-8c22-cfd6312adae3', intentional_termination_categorically_impermissible, deontological).
narrative_ontology:cs_reference_frame('d6ca8d8d-4270-464a-8c22-cfd6312adae3', sanctity_doctrine_framework).
narrative_ontology:cs_drift_state('d6ca8d8d-4270-464a-8c22-cfd6312adae3', contemporary_aid_in_dying_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('d6ca8d8d-4270-464a-8c22-cfd6312adae3', '').
narrative_ontology:cs_kernel_id(dignified_death__sanctity_primary, dignified_death).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dignified_death__sanctity_primary, moral_order_community).
narrative_ontology:constraint_victim(dignified_death__sanctity_primary, terminally_ill_patients_in_unbearable_suffering).
narrative_ontology:constraint_victim(dignified_death__sanctity_primary, elderly_with_advance_directives).
narrative_ontology:constraint_victim(dignified_death__sanctity_primary, severely_disabled_populations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(dignified_death__sanctity_primary, clinical_providers).
narrative_ontology:constraint_vindicates(dignified_death__sanctity_primary, transcendent_moral_law_doctrine).
narrative_ontology:constraint_vindicates(dignified_death__sanctity_primary, life_intrinsic_value_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Represented primarily through religious institutions, conservative bioethicists, and legislators who vindicate the sanctity doctrine. They argue that permitting intentional life-termination violates a transcendent moral order that transcends individual preference. Their authority derives from doctrinal interpretation and legislative codification of the sanctity principle. They set enforcement standards for medical practice and shape what constitutes dignified care.
narrative_ontology:constraint_stakeholder(dignified_death__sanctity_primary, moral_order_community, beneficiary,
    institutional, civilizational, analytical, universal).
narrative_ontology:stakeholder_secondary_role(dignified_death__sanctity_primary, moral_order_community, agenda_setter).

% Face the constraint through mandatory continuation of life-sustaining treatment despite unbearable pain and loss of function. They cannot exit the medical system without abandoning hope for any pain relief or comfort. Their exit options collapse entirely at the moment of terminal diagnosis—they cannot choose a time and manner of death aligned with their considered values, only when and whether to refuse treatment (if legal). Suffer extraction through prolonged dying that violates their own conception of dignity.
narrative_ontology:constraint_stakeholder(dignified_death__sanctity_primary, terminally_ill_patients_in_unbearable_suffering, payer,
    powerless, immediate, trapped, local).

% Have documented their wishes to refuse life-prolonging intervention under specified conditions, but the sanctity constraint prevents execution of those directives in many jurisdictions. They face identity-lock through accumulated social messaging that choosing death is 'selfish' or 'faithless,' even when exercising documented autonomy. Their moral identity becomes fused with the obligation to continue living—refusing the constraint means not only medical conflict but perceived moral failure.
narrative_ontology:constraint_stakeholder(dignified_death__sanctity_primary, elderly_with_advance_directives, payer,
    moderate, biographical, identity_locked, regional).

% Experience the constraint as a presumption that their lives are worth prolonging regardless of their own judgment. Medical systems operate under sanctity-based presumptions that assume disabled people should want to continue living, creating barriers to respecting refusals of treatment. They are subject to coercion through paternalistic override of stated preferences. Their constrained exit options are further constrained by the legal doctrine's presumption against their autonomous choice.
narrative_ontology:constraint_stakeholder(dignified_death__sanctity_primary, severely_disabled_populations, payer,
    powerless, biographical, constrained, national).

% Are positioned as enforcers of the sanctity constraint through medical licensing boards, hospital ethics committees, and criminal law. They must continue life-sustaining treatment even when clinically futile or explicitly refused by the patient. They face professional and legal penalties for honoring patient end-of-life wishes that violate the sanctity doctrine. Their constraint is reinforced through peer enforcement and licensure vulnerability.
narrative_ontology:constraint_stakeholder(dignified_death__sanctity_primary, clinical_providers, payer,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(dignified_death__sanctity_primary, clinical_providers, agenda_setter).

% Are formally excluded from decision authority in sanctity-enforcing jurisdictions and must witness prolonged suffering they could mediate if the constraint permitted. They experience moral distress watching a dying relative suffer when the family collectively judges dignified death would be preferable. Their exclusion from a 'relational autonomy' framing (the sibling reading) is structural to this reading's operation.
narrative_ontology:constraint_stakeholder(dignified_death__sanctity_primary, family_members_of_dying, excluded,
    moderate, biographical, constrained, local).

% Have adopted the autonomy or relational-autonomy readings and created space for aid-in-dying practices. Their existence creates a counterexample that directly contradicts the sanctity reading's universalist claims, and they attract cross-border exit (medical tourism) from jurisdictions enforcing the sanctity constraint. They represent a feasible institutional alternative that proves the constraint is not natural law but legal choice.
narrative_ontology:constraint_stakeholder(dignified_death__sanctity_primary, jurisdictions_legalizing_aid_in_dying, excluded,
    powerful, generational, arbitrage, global).

% Produce and propagate the intellectual authority for the sanctity reading through academic bioethics, advisory bodies to legislatures, and medical education. They translate doctrinal claims into policy-ready arguments and serve as expert witnesses in right-to-die litigation. They maintain the conceptual framework that vindicates the constraint.
narrative_ontology:constraint_stakeholder(dignified_death__sanctity_primary, conservative_bioethicists, agenda_setter,
    institutional, generational, analytical, global).

% Occupy an ambiguous position: some endorse aggressive palliative care as compatible with sanctity (reducing suffering within the constraint), while others argue the constraint prevents adequate pain relief and autonomy in dying. Their exclusion from the agenda-setting seat means their clinical evidence about what patients actually experience rarely reshapes the sanctity doctrine's application.
narrative_ontology:constraint_stakeholder(dignified_death__sanctity_primary, palliative_care_advocates, excluded,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(dignified_death__sanctity_primary, palliative_care_advocates, observer).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(dignified_death__sanctity_primary, moral_order_community).
narrative_ontology:fixing_cost_class(dignified_death__sanctity_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a universal moral boundary against intentional life-ending, coordinating medical practice and law around the proposition that human life possesses transcendent value that cannot be canceled by suffering or consent. Solves the problem of protecting the vulnerable from coercion to die by making such coercion categorically impermissible.
% TRANSFER_FUNCTION: Moves the burden of continued living (including unbearable suffering, loss of autonomy, medical intervention the patient rejects) from the individual patient to the patient, and redistributes moral authority from the patient's judgment to the institutional order (medical boards, legislatures, religious authorities) that enforces the sanctity doctrine. Transfers decision power away from the dying individual toward the regime that vindicates transcendent moral law.
% ABSENT_VOICES: Patients whose considered values conflict with the sanctity doctrine are structurally excluded from setting the terms of their own dying. Aid-in-dying advocates and autonomy-primary patients cannot participate in the jurisdictions that enforce sanctity—their voices are not in the room where the rules are set, they are only subjects of them. Jurisdictions that have legalized aid-in-dying and their patients represent an absent counter-reading that is actively suppressed by sanctity-enforcing regimes.
% DISAPPEARANCE_RATIONALE: If the sanctity constraint disappeared, medical practice in sanctity-enforcing jurisdictions would immediately shift: patients with end-of-life directives would see those honored, aid-in-dying options would open in some contexts, clinical focus would shift from life-prolongation to patient-aligned decision-making. The institutional architecture of medical licensure, criminal prohibition, and hospital policy would require rapid reorganization. Medical tourism from restrictive to permissive jurisdictions would decline. This is not a natural rearrangement—it requires active institutional change—but the world would substantially reorganize.
% FOUNDING_PROBLEM: The founding problem is protection of vulnerable people from coercive end-of-life decisions driven by economic pressure (healthcare costs), family burden, or social devaluation of disability. The sanctity doctrine aims to create an absolute floor: no circumstance—poverty, suffering, family preference, disability status—can justify intentionally ending a human life.
% FOUNDING_PROBLEM_CORROBORATION: Sanctity advocates attest the problem remains live, citing cases where economic or social pressure leads families toward end-of-life decisions. Autonomy advocates and aid-in-dying jurisdictions counter-attest that the founding problem has been substantially addressed by palliative care advances, informed-consent law, and explicit procedural safeguards in aid-in-dying statutes, and that the constraint now perpetuates suffering rather than preventing coercion. Independent comparative analysis from jurisdictions with different regulatory regimes (e.g. Netherlands, Canada, US) shows vulnerable populations do NOT face systematically higher rates of aid-in-dying when legalized under procedural safeguards—contradicting the founding problem's continued urgency. This corroboration comes from outside the sanctity-benefiting institutions.
narrative_ontology:disappearance_verdict(dignified_death__sanctity_primary, world_rearranges).
narrative_ontology:founding_problem_status(dignified_death__sanctity_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dignified_death__sanctity_primary, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(dignified_death__sanctity_primary, 'none', 1).
narrative_ontology:epsilon_provenance(dignified_death__sanctity_primary, 0.58, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is 0.58 (not maximal) because the constraint does carry a coordination function—it establishes a universal moral boundary intended to protect the vulnerable from coercive end-of-life pressure. However, the empirical corroboration for the founding problem's continued urgency is weak: palliative care has advanced substantially, aid-in-dying jurisdictions with procedural safeguards show no higher rates of aid-in-dying in vulnerable populations, and the constraint's operation now manifestly extracts suffering from the terminally ill whose preferences it overrides. Suppression is high (0.71) because enforcement depends on active legal prohibition, medical licensing sanctions, and criminal penalties—the constraint cannot persist through preference alignment alone; it requires institutional force. Theater is moderate (0.42): while the constraint is presented as protecting vulnerable people, clinical observation shows an increasing share of institutional activity defends the prohibition itself rather than providing palliative alternatives. Accessibility collapse is high (0.68) because once a patient is terminally ill and in unbearable suffering, the alternatives to continuing life-sustaining treatment collapse almost completely—exit is trapped. The measurements show extractiveness increasing slightly from t=0 to t=20 (as aid-in-dying has become more salient and sanctity enforcement has intensified), then plateauing at 0.58-0.59, suggesting the constraint has reached equilibrium pressure. Suppression follows a similar trajectory, indicating enforcement infrastructure is stable rather than escalating.
 *
 * PERSPECTIVAL GAP:
 *   From the moral_order_community's agenda-setting seat, this constraint is a pure coordination solution: it establishes a universal moral principle that protects all persons, especially the vulnerable, from commodification and coercive pressure. The constraint's persistence and enforcement appear as necessary institutional maintenance of transcendent truth. From the terminally_ill_patients_in_unbearable_suffering seat, the same structure operates as coercive extraction: the constraint denies them a choice that aligns with their most deeply held values and perpetuates suffering they judge to be undignified. From the clinical_providers seat, the constraint is experienced as conflicting professional duties: their training emphasizes patient autonomy and relief of suffering, but the legal regime commands life-prolongation. The engine computes these seat-divergent classifications from the structural data: different power levels (institutional vs. powerless), different exit options (analytical vs. trapped), different role positions (agenda-setter vs. payer). The authored claim (snare) and the authored metrics (high suppression, moderate-high extractiveness, significant theater) reflect the sanctity reading's actual operation—not a harmonized consensus but a regime that benefits one seat while constraining others.
 *
 * DIRECTIONALITY LOGIC:
 *   The moral_order_community sits at d ≈ 0.0–0.1 (full beneficiary): it collects the direct benefit (vindication of sacred doctrine, institutional authority over life decisions), faces negligible costs, and has arbitrage-grade exit options (it can shift its reading to autonomy framing, as some religious traditions have done, or maintain sanctity in alternative jurisdictions). Terminally_ill_patients_in_unbearable_suffering sit at d ≈ 0.95–1.0 (full target): they bear the extraction (forced continuation of suffering), have trapped exit (no choice once terminal diagnosis is rendered), and benefit negligibly if at all from the constraint. Elderly_with_advance_directives sit at d ≈ 0.75–0.85 (near-target): they bear substantial extraction (denial of documented preference), have identity-locked exit (fused with social/moral obligation to continue living), and moderate power that does not translate to exit capacity. Severely_disabled_populations sit at d ≈ 0.80–0.90 (near-target): they bear paternalistic override, have constrained exit amplified by the constraint's presumption against their judgment, and are powerless. Clinical_providers sit at d ≈ 0.55–0.65 (near-symmetric but tending toward target): they bear suppression through licensing/criminal penalties and experience professional distress, but maintain moderate power and biographical time horizons that offer some exit (career change, jurisdiction shift, civil disobedience). Family_members_excluded sit at d ≈ 0.60–0.70 (tending target): they bear moral distress and witness suffering they could mediate, but are not the direct subjects of the constraint, and have greater institutional resources than the dying patient.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is protection from coercive end-of-life pressure on vulnerable people. The constraint's operation should directly measure against this problem: does the sanctity rule prevent coercion? Comparative international analysis shows that in jurisdictions that legalized aid-in-dying with procedural safeguards (Netherlands, Belgium, Oregon, Canada), vulnerable populations (elderly, disabled, poor) do NOT receive aid-in-dying at systematically higher rates than affluent populations—contradicting the founding problem's predicted mechanism. Meanwhile, in sanctity-enforcing jurisdictions, there is documented evidence of patients suffering against their stated preferences, of family distress, and of clinical providers experiencing moral injury from enforced life-prolongation. The founding_problem_status is 'contested' because beneficiary institutions attests it remains live (pointing to isolated cases of economic or family pressure), while the systemic data and relational-autonomy jurisdictions attest it has been substantially addressed by procedural law and palliative care. This constraint exhibits mandatrophy: the rule persists not because it solves the founding problem more effectively than alternatives, but because institutional authority benefits from maintaining it. The theater_ratio rising from 0.35 to 0.43 indicates increasing proportional effort devoted to defending the prohibition itself (regulatory enforcement, legal argument) rather than delivering palliative care or supporting patients' values-aligned choices—classic Goodhart drift.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    founding_problem_persistence,
    'Does the founding problem—vulnerability to coercive end-of-life pressure—remain empirically live in jurisdictions with aid-in-dying under procedural safeguards, or has it been substantially addressed by legal and palliative-care advances?',
    'Comparative longitudinal analysis of aid-in-dying patterns across jurisdictions with different regulatory regimes, controlling for demographics (age, disability status, economic status). If vulnerable populations receive aid-in-dying at no higher rates in permissive jurisdictions than affluent populations, the founding problem is addressable without the sanctity constraint. If vulnerable populations receive aid-in-dying at systematically higher rates under identical procedural standards, the founding problem persists and the sanctity constraint has legitimate protective force.',
    'If the founding problem has been addressed, the constraint''s persistence becomes mandatrophy—it perpetuates suffering without solving the problem it was built for. This would move the type classification toward piton (atrophied function, inertial persistence). If the founding problem remains live despite safeguards, the constraint''s protective function is vindicated and type classification edges back toward tangled_rope (genuine coordination with asymmetric extraction).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(founding_problem_persistence, empirical, 'Whether the constraint''s founding protective problem remains live or has been addressed by procedural and palliative-care alternatives.').

omega_variable(
    transcendent_moral_law_grounding,
    'Is the sanctity doctrine''s claim to transcendent moral law a genuine metaphysical commitment or an institutional framing used to stabilize the constraint against competing readings?',
    'Historical and genealogical analysis: does the doctrine have consistent metaphysical foundations across religious traditions, or does it vary and adapt to institutional and jurisdictional requirements? Within traditions that hold sanctity doctrine, do they maintain internal consistency when confronted with competing values (e.g., suicide under torture, capital punishment, warfare)? If inconsistency is widespread, the transcendent grounding is precarious and the doctrine is more accurately described as institutional power assertion than discovery of objective moral truth.',
    'If transcendent grounding is genuine and consistent, the constraint carries legitimacy that transcends jurisdictional disagreement and democratic processes—it is not a policy choice but a truth claim. If grounding is inconsistent or instrumental, the constraint is revealed as institutional assertion, which reframes the extraction: it becomes clearer that beneficiary institutions are extracting authority from vulnerable patients to maintain a doctrine that serves the institutions'' power. This does not change type classification but deepens the extraction analysis.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transcendent_moral_law_grounding, conceptual, 'Whether sanctity doctrine grounds in consistent transcendent truth or in institutional power assertion.').

omega_variable(
    structural_necessity_of_prohibition,
    'Is the categorical prohibition on aid-in-dying structurally necessary to achieve the constraint''s protective aims, or could the aims be achieved through procedural safeguards, informed consent, and palliative care advances while permitting aid-in-dying?',
    'Institutional comparison: do jurisdictions that permit aid-in-dying under procedural safeguards achieve comparable or better outcomes in protecting vulnerable populations, relieving suffering, and maintaining dignity, relative to prohibition-based regimes? Counterfactual: if a sanctity jurisdiction adopted procedural safeguards while lifting the categorical prohibition, would protective outcomes degrade?',
    'If procedural alternatives achieve equal or superior protective outcomes, the prohibition becomes unnecessary constraint—extraction without justifying benefit. This would support reclassification from snare (extraction with protective cover) to pure snare (extraction defended only by institutional authority). If protection genuinely depends on categorical prohibition, the extraction is justified protective cost and type edges toward tangled_rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(structural_necessity_of_prohibition, empirical, 'Whether the constraint''s protective function requires categorical prohibition or is achievable through procedural safeguards and advanced palliative care.').

omega_variable(
    internalized_vs_structural_suppression,
    'Is the observed suppression of end-of-life choices primarily structural (external barriers: legal prohibition, institutional policy, clinical access denial) or internalized (the patient has absorbed the sanctity doctrine and believes choosing death is morally impermissible)?',
    'Comparison of stated preferences pre- vs. post-legalization in jurisdictions that shifted from prohibition to procedural legalization. If patients in newly-permissive jurisdictions express end-of-life wishes they previously suppressed, suppression was partly internalized. If adoption rates stabilize quickly at low levels despite legalization, suppression was primarily structural (the external barrier was the binding constraint, and removing it reveals limited actual demand). Post-exit trajectory analysis: do patients who exit sanctity-enforcing jurisdictions to access aid-in-dying in permissive jurisdictions report subjective experience of suppression-lifting, or do they report the choice was driven by other factors?',
    'If suppression is primarily internalized (patients have fused their moral identity with sanctity), the constraint is durable even if externally removed—removal does not restore autonomy because the person no longer trusts their own judgment. If suppression is primarily structural, removing the prohibition should show measurable preference revelation. The distinction affects estimates of constraint durability and the feasibility of alternative governance structures.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_vs_structural_suppression, empirical, 'Whether suppression of end-of-life autonomy is structural (external) or internalized (cognitive/identity-level).').

omega_variable(
    contested_reading_coverage,
    'Do the three declared readings (sanctity_primary, autonomy_primary, relational_autonomy) exhaustively partition the live positions in the dignified_death kernel, or are there additional readings omitted?',
    'Systematic canvassing of bioethics literature, legislative debates, and cross-cultural end-of-life ethics frameworks. Does a fourth reading emerge (e.g., communitarian dignity grounded in role and kinship rather than individual transcendence or individual autonomy)? Do any of the three readings fail to capture a coherent, live position held by identifiable institutional or cultural actors?',
    'If additional readings exist and are omitted, the constraint corpus underspecifies the kernel contest. The three readings might not actually foreclose/influence/coexist with one another as specified in cs_structure.reading_relations; a fourth reading might cross-cut the relationships. If a communitarian reading exists and coexists with all three, the network of relationships becomes more complex. If the three readings are exhaustive and mutually exclusive for any single institutional framework, the reading_relations are stable.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(contested_reading_coverage, conceptual, 'Whether three readings exhaustively partition live positions in dignified_death kernel or whether additional readings exist.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dignified_death__sanctity_primary, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dign_tr_t0, dignified_death__sanctity_primary, theater_ratio, 0, 0.35).
narrative_ontology:measurement_basis(dign_tr_t0, observed).
narrative_ontology:measurement(dign_tr_t5, dignified_death__sanctity_primary, theater_ratio, 5, 0.37).
narrative_ontology:measurement_basis(dign_tr_t5, observed).
narrative_ontology:measurement(dign_tr_t10, dignified_death__sanctity_primary, theater_ratio, 10, 0.39).
narrative_ontology:measurement_basis(dign_tr_t10, observed).
narrative_ontology:measurement(dign_tr_t15, dignified_death__sanctity_primary, theater_ratio, 15, 0.41).
narrative_ontology:measurement_basis(dign_tr_t15, observed).
narrative_ontology:measurement(dign_tr_t20, dignified_death__sanctity_primary, theater_ratio, 20, 0.42).
narrative_ontology:measurement_basis(dign_tr_t20, observed).
narrative_ontology:measurement(dign_tr_t25, dignified_death__sanctity_primary, theater_ratio, 25, 0.43).
narrative_ontology:measurement_basis(dign_tr_t25, projected).
narrative_ontology:measurement(dign_tr_t30, dignified_death__sanctity_primary, theater_ratio, 30, 0.43).
narrative_ontology:measurement_basis(dign_tr_t30, projected).
narrative_ontology:measurement(dign_tr_t40, dignified_death__sanctity_primary, theater_ratio, 40, 0.42).
narrative_ontology:measurement_basis(dign_tr_t40, projected).

% Extraction over time
narrative_ontology:measurement(dign_be_t0, dignified_death__sanctity_primary, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(dign_be_t0, observed).
narrative_ontology:measurement(dign_be_t5, dignified_death__sanctity_primary, base_extractiveness, 5, 0.5).
narrative_ontology:measurement_basis(dign_be_t5, observed).
narrative_ontology:measurement(dign_be_t10, dignified_death__sanctity_primary, base_extractiveness, 10, 0.54).
narrative_ontology:measurement_basis(dign_be_t10, observed).
narrative_ontology:measurement(dign_be_t15, dignified_death__sanctity_primary, base_extractiveness, 15, 0.56).
narrative_ontology:measurement_basis(dign_be_t15, observed).
narrative_ontology:measurement(dign_be_t20, dignified_death__sanctity_primary, base_extractiveness, 20, 0.58).
narrative_ontology:measurement_basis(dign_be_t20, observed).
narrative_ontology:measurement(dign_be_t25, dignified_death__sanctity_primary, base_extractiveness, 25, 0.59).
narrative_ontology:measurement_basis(dign_be_t25, projected).
narrative_ontology:measurement(dign_be_t30, dignified_death__sanctity_primary, base_extractiveness, 30, 0.59).
narrative_ontology:measurement_basis(dign_be_t30, projected).
narrative_ontology:measurement(dign_be_t40, dignified_death__sanctity_primary, base_extractiveness, 40, 0.58).
narrative_ontology:measurement_basis(dign_be_t40, projected).

% Suppression requirement over time
narrative_ontology:measurement(dign_su_t0, dignified_death__sanctity_primary, suppression_requirement, 0, 0.62).
narrative_ontology:measurement_basis(dign_su_t0, observed).
narrative_ontology:measurement(dign_su_t5, dignified_death__sanctity_primary, suppression_requirement, 5, 0.64).
narrative_ontology:measurement_basis(dign_su_t5, observed).
narrative_ontology:measurement(dign_su_t10, dignified_death__sanctity_primary, suppression_requirement, 10, 0.66).
narrative_ontology:measurement_basis(dign_su_t10, observed).
narrative_ontology:measurement(dign_su_t15, dignified_death__sanctity_primary, suppression_requirement, 15, 0.68).
narrative_ontology:measurement_basis(dign_su_t15, observed).
narrative_ontology:measurement(dign_su_t20, dignified_death__sanctity_primary, suppression_requirement, 20, 0.7).
narrative_ontology:measurement_basis(dign_su_t20, observed).
narrative_ontology:measurement(dign_su_t25, dignified_death__sanctity_primary, suppression_requirement, 25, 0.71).
narrative_ontology:measurement_basis(dign_su_t25, projected).
narrative_ontology:measurement(dign_su_t30, dignified_death__sanctity_primary, suppression_requirement, 30, 0.71).
narrative_ontology:measurement_basis(dign_su_t30, projected).
narrative_ontology:measurement(dign_su_t40, dignified_death__sanctity_primary, suppression_requirement, 40, 0.7).
narrative_ontology:measurement_basis(dign_su_t40, projected).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=0, tn=40
narrative_ontology:measurement(dign_grid_01, dignified_death__sanctity_primary, accessibility_collapse(class), 0, 0.65).
narrative_ontology:measurement(dign_grid_02, dignified_death__sanctity_primary, accessibility_collapse(class), 40, 0.68).
narrative_ontology:measurement(dign_grid_03, dignified_death__sanctity_primary, accessibility_collapse(individual), 0, 0.85).
narrative_ontology:measurement(dign_grid_04, dignified_death__sanctity_primary, accessibility_collapse(individual), 40, 0.87).
narrative_ontology:measurement(dign_grid_05, dignified_death__sanctity_primary, accessibility_collapse(organizational), 0, 0.72).
narrative_ontology:measurement(dign_grid_06, dignified_death__sanctity_primary, accessibility_collapse(organizational), 40, 0.74).
narrative_ontology:measurement(dign_grid_07, dignified_death__sanctity_primary, accessibility_collapse(structural), 0, 0.58).
narrative_ontology:measurement(dign_grid_08, dignified_death__sanctity_primary, accessibility_collapse(structural), 40, 0.61).
narrative_ontology:measurement(dign_grid_09, dignified_death__sanctity_primary, resistance(class), 0, 0.61).
narrative_ontology:measurement(dign_grid_10, dignified_death__sanctity_primary, resistance(class), 40, 0.65).
narrative_ontology:measurement(dign_grid_11, dignified_death__sanctity_primary, resistance(individual), 0, 0.58).
narrative_ontology:measurement(dign_grid_12, dignified_death__sanctity_primary, resistance(individual), 40, 0.62).
narrative_ontology:measurement(dign_grid_13, dignified_death__sanctity_primary, resistance(organizational), 0, 0.68).
narrative_ontology:measurement(dign_grid_14, dignified_death__sanctity_primary, resistance(organizational), 40, 0.72).
narrative_ontology:measurement(dign_grid_15, dignified_death__sanctity_primary, resistance(structural), 0, 0.52).
narrative_ontology:measurement(dign_grid_16, dignified_death__sanctity_primary, resistance(structural), 40, 0.55).
narrative_ontology:measurement(dign_grid_17, dignified_death__sanctity_primary, stakes_inflation(class), 0, 0.68).
narrative_ontology:measurement(dign_grid_18, dignified_death__sanctity_primary, stakes_inflation(class), 40, 0.7).
narrative_ontology:measurement(dign_grid_19, dignified_death__sanctity_primary, stakes_inflation(individual), 0, 0.88).
narrative_ontology:measurement(dign_grid_20, dignified_death__sanctity_primary, stakes_inflation(individual), 40, 0.89).
narrative_ontology:measurement(dign_grid_21, dignified_death__sanctity_primary, stakes_inflation(organizational), 0, 0.74).
narrative_ontology:measurement(dign_grid_22, dignified_death__sanctity_primary, stakes_inflation(organizational), 40, 0.75).
narrative_ontology:measurement(dign_grid_23, dignified_death__sanctity_primary, stakes_inflation(structural), 0, 0.62).
narrative_ontology:measurement(dign_grid_24, dignified_death__sanctity_primary, stakes_inflation(structural), 40, 0.64).
narrative_ontology:measurement(dign_grid_25, dignified_death__sanctity_primary, suppression(class), 0, 0.62).
narrative_ontology:measurement(dign_grid_26, dignified_death__sanctity_primary, suppression(class), 40, 0.63).
narrative_ontology:measurement(dign_grid_27, dignified_death__sanctity_primary, suppression(individual), 0, 0.76).
narrative_ontology:measurement(dign_grid_28, dignified_death__sanctity_primary, suppression(individual), 40, 0.77).
narrative_ontology:measurement(dign_grid_29, dignified_death__sanctity_primary, suppression(organizational), 0, 0.68).
narrative_ontology:measurement(dign_grid_30, dignified_death__sanctity_primary, suppression(organizational), 40, 0.69).
narrative_ontology:measurement(dign_grid_31, dignified_death__sanctity_primary, suppression(structural), 0, 0.58).
narrative_ontology:measurement(dign_grid_32, dignified_death__sanctity_primary, suppression(structural), 40, 0.59).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dignified_death__sanctity_primary, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(dignified_death__sanctity_primary, 0.12).
narrative_ontology:affects_constraint(dignified_death__sanctity_primary, dignified_death__autonomy_primary).
narrative_ontology:affects_constraint(dignified_death__sanctity_primary, dignified_death__relational_autonomy).
narrative_ontology:affects_constraint(dignified_death__sanctity_primary, palliative_care_institutional_capacity).
narrative_ontology:affects_constraint(dignified_death__sanctity_primary, medical_futility_determination).

% DUAL FORMULATION NOTE:
% The dignified_death kernel is instantiated by three structurally distinct constraint stories: sanctity_primary (this story, ε≈0.58, snare type, victims=terminally_ill), autonomy_primary (ε≈0.45–0.55, tangled_rope or rope type, victims=those overridden by majoritarian will), and relational_autonomy (ε≈0.30–0.40, rope or scaffold type, victims=those excluded from relational process). The readings cannot be authored as a single story: they carry materially different victim sets, beneficiary structures, and measured extractiveness. Each reading is a complete ε-invariant constraint. The network links capture: (1) sanctity→autonomy: sanctity prohibition structurally forecloses autonomy reading's policy implementation in sanctity-enforcing jurisdictions (if sanctity wins legislatively, autonomy cannot operate there); (2) autonomy→relational: autonomy reading influences relational reading by creating structural pressure to add safeguards; (3) relational≠sanctity: relational coexists with sanctity in different jurisdictions and within pluralistic bioethics. All three readings influence the palliative_care and medical_futility constraints because they differ in when palliative care is deemed adequate and when futility is declared.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
