% ============================================================================
% CONSTRAINT STORY: end_of_life_authority__autonomy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: end_of_life_authority__autonomy_reading
 *   human_readable: Autonomy-Based Right to Assisted Death for Unbearable Suffering
 *   domain: medical_ethics/bioethics/end_of_life_policy
 *
 * SUMMARY:
 *   The autonomy reading of end-of-life authority asserts that individual
 *   self-determination grounds a right to medical assistance in dying when
 *   suffering is unbearable. This reading operates as a tangled rope: it
 *   coordinates a genuine collective-action problem (replacing clandestine
 *   suffering and suicide with a regulated pathway) while extracting
 *   asymmetric costs from those denied access (the suffering_prolonged),
 *   clinicians compelled to participate or refer, and families bearing
 *   witness to legally mandated prolongation. The constraint requires active
 *   enforcement — eligibility assessments, waiting periods, clinician
 *   oversight, prosecutorial discretion — to maintain its boundary against
 *   both the sanctity prohibition and the slippery_slope expansion. Over the
 *   interval (0–30 years of operation in early-adopter jurisdictions), base
 *   extractiveness rises modestly as eligibility expands beyond the founding
 *   terminal-illness frame; suppression requirement falls as the constraint
 *   normalizes and enforcement shifts from prohibition to regulation; theater
 *   ratio rises slightly as procedural safeguards accumulate performative
 *   compliance elements. The claim/metric independence is deliberate: the
 *   reading claims to be a pure coordination mechanism (rope), but the
 *   authored metrics reveal asymmetric extraction and enforcement dependence
 *   (tangled_rope).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(end_of_life_authority__autonomy_reading, 0.18).
domain_priors:suppression_score(end_of_life_authority__autonomy_reading, 0.72).
domain_priors:theater_ratio(end_of_life_authority__autonomy_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(end_of_life_authority__autonomy_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(end_of_life_authority__autonomy_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(end_of_life_authority__autonomy_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(end_of_life_authority__autonomy_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(end_of_life_authority__autonomy_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(end_of_life_authority__autonomy_reading, tangled_rope).
narrative_ontology:human_readable(end_of_life_authority__autonomy_reading, "Autonomy-Based Right to Assisted Death for Unbearable Suffering").
narrative_ontology:topic_domain(end_of_life_authority__autonomy_reading, "medical_ethics/bioethics/end_of_life_policy").

domain_priors:requires_active_enforcement(end_of_life_authority__autonomy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(end_of_life_authority__autonomy_reading, '9b8a8f9d-5b7f-4bc2-8f85-0073b45b0ada').
narrative_ontology:cs_kernel_codification('9b8a8f9d-5b7f-4bc2-8f85-0073b45b0ada', formalized).
narrative_ontology:cs_authority_grounding('9b8a8f9d-5b7f-4bc2-8f85-0073b45b0ada', lineage).
narrative_ontology:cs_interpretation_layer_present('9b8a8f9d-5b7f-4bc2-8f85-0073b45b0ada').
narrative_ontology:cs_reading_relation('9b8a8f9d-5b7f-4bc2-8f85-0073b45b0ada', end_of_life_authority__sanctity_reading, coexists_with).
narrative_ontology:cs_reading_relation('9b8a8f9d-5b7f-4bc2-8f85-0073b45b0ada', end_of_life_authority__slippery_slope_mechanism, influences).
narrative_ontology:cs_axiom('9b8a8f9d-5b7f-4bc2-8f85-0073b45b0ada', foundational, bodily_self_determination_includes_death_timing).
narrative_ontology:cs_axiom_status(bodily_self_determination_includes_death_timing, holdable).
narrative_ontology:cs_axiom_grounding('9b8a8f9d-5b7f-4bc2-8f85-0073b45b0ada', bodily_self_determination_includes_death_timing, deontological).
narrative_ontology:cs_axiom('9b8a8f9d-5b7f-4bc2-8f85-0073b45b0ada', foundational, unbearable_suffering_grounds_relief_claim).
narrative_ontology:cs_axiom_status(unbearable_suffering_grounds_relief_claim, holdable).
narrative_ontology:cs_axiom_grounding('9b8a8f9d-5b7f-4bc2-8f85-0073b45b0ada', unbearable_suffering_grounds_relief_claim, deontological).
narrative_ontology:cs_reference_frame('9b8a8f9d-5b7f-4bc2-8f85-0073b45b0ada', classical_prohibition_framework).
narrative_ontology:cs_drift_state('9b8a8f9d-5b7f-4bc2-8f85-0073b45b0ada', contemporary_autonomy_expansion_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('9b8a8f9d-5b7f-4bc2-8f85-0073b45b0ada', '').
narrative_ontology:cs_kernel_id(end_of_life_authority__autonomy_reading, end_of_life_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(end_of_life_authority__autonomy_reading, terminally_ill_patients).
narrative_ontology:constraint_beneficiary(end_of_life_authority__autonomy_reading, patients_with_unbearable_suffering).
narrative_ontology:constraint_beneficiary(end_of_life_authority__autonomy_reading, autonomy_advocacy_organizations).
narrative_ontology:constraint_victim(end_of_life_authority__autonomy_reading, suffering_prolonged_patients).
narrative_ontology:constraint_victim(end_of_life_authority__autonomy_reading, families_of_suffering_patients).
narrative_ontology:constraint_victim(end_of_life_authority__autonomy_reading, clinicians_forced_into_complicity).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(end_of_life_authority__autonomy_reading, palliative_care_specialists).
narrative_ontology:constraint_vindicates(end_of_life_authority__autonomy_reading, bodily_self_determination_principle).
narrative_ontology:constraint_vindicates(end_of_life_authority__autonomy_reading, relief_of_suffering_as_medical_goal).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Face terminal diagnoses with predictable suffering trajectories. The autonomy reading grants them legal access to assisted death, converting what would be prolonged dying into a chosen exit. They must navigate eligibility assessments, waiting periods, and clinician availability — exit is legally possible but practically constrained.
narrative_ontology:constraint_stakeholder(end_of_life_authority__autonomy_reading, terminally_ill_patients, beneficiary,
    moderate, immediate, constrained, national).

% Include non-terminal patients with severe chronic conditions (neurodegenerative, psychiatric, intractable pain) whose suffering meets the 'unbearable' threshold. In jurisdictions where eligibility has expanded, they gain access; where it has not, they remain in the suffering_prolonged victim set. Their exit depends on legislative and judicial interpretation of 'unbearable.'
narrative_ontology:constraint_stakeholder(end_of_life_authority__autonomy_reading, patients_with_unbearable_suffering, beneficiary,
    moderate, immediate, constrained, national).

% Civil society groups (e.g., Dying With Dignity, Exit International) that lobby for legislative change, fund litigation, and provide navigational support to patients. They gain organizational legitimacy, membership, and policy influence from the constraint's operation. Their exit is mobile — they can shift focus to other rights campaigns.
narrative_ontology:constraint_stakeholder(end_of_life_authority__autonomy_reading, autonomy_advocacy_organizations, beneficiary,
    organized, generational, mobile, national).

% Patients who meet the autonomy reading's moral criteria for assisted death but are denied access by current law — either because their jurisdiction has not adopted the reading, or because eligibility criteria exclude their condition (e.g., psychiatric suffering, advance dementia, non-terminal chronic illness). They endure suffering that the reading says should be controllable. No legal exit exists; extralegal exit (suicide, travel) carries high risk and cost.
narrative_ontology:constraint_stakeholder(end_of_life_authority__autonomy_reading, suffering_prolonged_patients, payer,
    powerless, immediate, trapped, national).

% Bear witness to prolonged suffering of loved ones when assisted death is denied. Absorb emotional, financial, and caregiving burdens that the autonomy reading would alleviate. Some face legal jeopardy for assisting extralegal exits. Their exit is constrained — they cannot 'leave' the family situation, but can advocate or relocate.
narrative_ontology:constraint_stakeholder(end_of_life_authority__autonomy_reading, families_of_suffering_patients, payer,
    powerless, biographical, constrained, national).

% Physicians and nurses who object to assisted death on conscience grounds but practice in jurisdictions where the autonomy reading is law. They must either refer (which they may view as complicity), exit the specialty, or risk disciplinary action. Their exit is constrained — leaving obstetrics, oncology, or palliative care is professionally costly; conscientious objection protections vary.
narrative_ontology:constraint_stakeholder(end_of_life_authority__autonomy_reading, clinicians_forced_into_complicity, payer,
    moderate, biographical, constrained, national).

% Clinical gatekeepers who assess eligibility, manage suffering, and implement or decline assisted death requests. They shape the constraint's operation through interpretation of 'unbearable suffering,' capacity assessments, and referral pathways. They benefit professionally from the expanded clinical infrastructure around assisted death. Their exit is mobile — they can shift to pure palliative practice where the constraint does not operate.
narrative_ontology:constraint_stakeholder(end_of_life_authority__autonomy_reading, palliative_care_specialists, agenda_setter,
    organized, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(end_of_life_authority__autonomy_reading, palliative_care_specialists, beneficiary).

% Write and interpret the laws that instantiate the autonomy reading. Define eligibility criteria, procedural safeguards, and clinician obligations. They respond to advocacy pressure, court challenges, and public opinion. Their exit is arbitrage — they can move between legislative, judicial, and academic roles.
narrative_ontology:constraint_stakeholder(end_of_life_authority__autonomy_reading, legislators_and_judges, agenda_setter,
    institutional, generational, arbitrage, national).

% Religious organizations, disability rights groups, and bioethicists who argue that intentional life-ending is intrinsically wrong. They are structurally excluded from the constraint's beneficiary logic — their moral framework is treated as an obstacle to be overridden rather than a position to be accommodated. They cannot exit the political contest without abandoning their core conviction.
narrative_ontology:constraint_stakeholder(end_of_life_authority__autonomy_reading, sanctity_of_life_advocates, excluded,
    organized, generational, constrained, national).

% Scholars who study the evolution of end-of-life law, track eligibility expansion patterns, and analyze the autonomy/sanctity/slippery_slope triangle. They neither collect nor pay; they map the structural dynamics. Their exit is analytical — they can redirect research attention.
narrative_ontology:constraint_stakeholder(end_of_life_authority__autonomy_reading, bioethics_observers, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a legally regulated pathway for individuals facing unbearable suffering to end their lives with medical assistance, replacing clandestine suicide, untreated suffering, and prosecutorial lottery with a transparent, assessed, and documented process.
% TRANSFER_FUNCTION: Transfers decisional authority over death timing from state prohibition and clinical paternalism to the suffering individual (subject to eligibility gates). Transfers clinical labor from life-prolongation to assisted-death provision. Transfers legal risk from patients and families to the state (which authorizes) and clinicians (who implement).
% ABSENT_VOICES: Future patients whose conditions will fall outside current eligibility but inside the autonomy reading's moral logic — e.g., advance dementia patients with prior directives, minors with terminal illness, psychiatric patients whose competence is contested. They are absent because they do not yet exist or cannot presently articulate a claim. Also absent: clinicians who left the profession rather than participate, whose silence is treated as resolution rather than exclusion.
% DISAPPEARANCE_RATIONALE: If the autonomy reading vanished overnight, jurisdictions with assisted death laws would revert to criminal prohibition. Patients currently using the pathway would lose legal access; clinicians would face prosecution risk; advocacy organizations would shift to civil disobedience or underground networks. The suffering_prolonged victim set would expand to include all current beneficiaries. The world rearranges because the constraint currently organizes a legally recognized transfer of authority and clinical labor.
% FOUNDING_PROBLEM: The absolute prohibition on assisted death forced individuals facing unbearable suffering into three untenable positions: clandestine violent suicide, prolonged suffering without relief, or dependence on clinician willingness to risk prosecution for mercy. The autonomy reading was built to replace this trilemma with a regulated, transparent, patient-controlled option.
% FOUNDING_PROBLEM_CORROBORATION: The autonomy reading's proponents (advocacy organizations, some bioethicists, legislative sponsors) attest the founding problem remains live — eligibility gaps persist, access is uneven, and new conditions (e.g., psychiatric suffering) remain excluded. Opponents (sanctity_advocates, some palliative care bodies, disability rights organizations) attest the founding problem is substantially solved where the reading operates, and further expansion constitutes a new problem rather than completion of the original one. Independent corroboration comes from longitudinal studies of jurisdictions with 20+ years of operation (Netherlands, Oregon, Belgium) showing steady eligibility expansion beyond the original terminal-illness frame — cited by both sides as evidence for their reading of the founding problem's status.
narrative_ontology:disappearance_verdict(end_of_life_authority__autonomy_reading, world_rearranges).
narrative_ontology:founding_problem_status(end_of_life_authority__autonomy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(end_of_life_authority__autonomy_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(end_of_life_authority__autonomy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(end_of_life_authority__autonomy_reading, 0.18, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness is low but nonzero (0.18) because the constraint's primary function is coordination (enabling chosen death) and the extraction falls on those excluded from eligibility — a structural asymmetry, not a transfer from beneficiaries to payers. Suppression is high (0.72) because the constraint's persistence depends on actively maintaining the legal boundary: without enforcement, the sanctity prohibition reasserts (criminalization) or the slippery_slope expansion proceeds unchecked (legislative drift). Theater ratio is low (0.12) — the clinical and legal machinery is functionally necessary, though expanding procedural checklists show early Goodhart drift. Accessibility collapse is moderate-high (0.68) because once the autonomy principle is accepted, alternatives (palliative sedation, voluntary stopping of eating/drinking, clandestine suicide) are structurally inferior for the coordination function. Resistance is moderate (0.58) from sanctity advocates, some clinician groups, and disability rights organizations — sufficient to contest expansion but not to repeal where established.
 *
 * PERSPECTIVAL GAP:
 *   From the beneficiary seats, the constraint is experienced as liberation — a rope that solves the coordination problem of dying well. From the suffering_prolonged seat, it is a snare — the autonomy principle is proclaimed but the gate remains closed. From the clinician_forced_into_complicity seat, it is a tangled rope — they coordinate the process they morally oppose. The engine computes this divergence from the structural data; the authored claim (tangled_rope) reflects the structural truth that all three experiences are simultaneously real.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (terminally_ill_patients, patients_with_unbearable_suffering, autonomy_advocacy_organizations) sit at low directionality (d ~ 0.15–0.25): the constraint subsidizes their agency. Victims (suffering_prolonged_patients, families, clinicians_forced_into_complicity) sit at high directionality (d ~ 0.75–0.90): the constraint extracts from them by denying access, imposing caregiving burdens, or compelling professional complicity. Palliative_care_specialists and legislators (agenda_setters) sit near symmetric (d ~ 0.45–0.55): they administer the constraint and bear its operational costs while gaining professional authority and political legitimacy. Excluded sanctity advocates are structurally locked out (d irrelevant — they are not governed by the constraint's logic). Observers sit at analytical (d = 0.5 by definition).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (replacing the prohibition trilemma) is partially solved where the reading operates but contested in scope. The constraint does not show mandatrophy — its function has not atrophied; rather, its eligibility boundary is the site of active contestation. Expansion to non-terminal and psychiatric conditions represents either completion of the autonomy logic (proponents) or slippery_slope drift (opponents). The classification prevents mislabeling: calling this a pure rope ignores the suffering_prolonged victim set; calling it a snare ignores the genuine coordination for beneficiaries. Tangled rope captures the hybrid structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    autonomy_sanctity_logical_incompatibility,
    'Does the autonomy reading''s core premise (self-determination grounds a right to assisted death) logically foreclose the sanctity reading''s core premise (intrinsic value of life prohibits intentional life-ending) within a single legal framework, or do they coexist as competing but structurally compatible positions?',
    'Constitutional court jurisprudence: if a framework can simultaneously recognize a right to assisted death AND protect conscientious objection for clinicians who hold the sanctity view, the readings coexist. If recognition of the right structurally requires the sanctity view to be legally irrelevant (no conscience protection, no institutional opt-out), the autonomy reading forecloses the sanctity reading.',
    'If forecloses, the kernel is structurally binary — jurisdictions must choose one reading as authoritative. If coexists_with, the kernel supports stable pluralism with contested boundaries. This determines whether the autonomy reading''s expansion is completion or drift.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(autonomy_sanctity_logical_incompatibility, conceptual, 'Whether the autonomy and sanctity readings are logically incompatible within one framework').

omega_variable(
    slippery_slope_as_structural_drift_vs_completion,
    'Is the documented expansion of eligibility criteria (terminal → non-terminal → psychiatric → advance directives → minors) a structural drift of the autonomy reading beyond its founding problem, or the logical completion of the autonomy principle itself?',
    'Longitudinal comparative analysis: if expansion correlates with weakening of safeguards (shorter waiting periods, fewer independent assessments, reduced clinician involvement), it signals drift. If expansion correlates with stable or strengthening safeguards, it signals principled completion. Track the safeguard-to-eligibility ratio over 30+ years in multiple jurisdictions.',
    'If drift, the autonomy reading shows mandatrophy — its founding problem is solved but the constraint expands via institutional momentum. If completion, the reading''s extractiveness on the suffering_prolonged is decreasing as eligibility catches up to the principle. The engine''s drift detection (T17) uses this distinction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(slippery_slope_as_structural_drift_vs_completion, empirical, 'Whether eligibility expansion represents drift or principled completion of the autonomy reading').

omega_variable(
    clinician_complicity_as_extraction_mechanism,
    'Is the burden on conscience-objecting clinicians a necessary coordination cost (they are the only ones who can assess and implement) or an extractive transfer (their moral injury is the price of the autonomy reading''s legitimacy)?',
    'Compare jurisdictions with strong vs. weak conscientious objection protections. If access metrics (wait times, geographic availability, patient satisfaction) are equivalent, the burden is extractive — strong protections would not impair coordination. If access degrades with strong protections, the burden is a coordination cost.',
    'If extractive, the clinician_forced_into_complicity victim set is a structural feature of the autonomy reading, not a bug — the reading requires moral injury to demonstrate state authority over the sanctity objection. This would raise the constraint''s effective extraction for that seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(clinician_complicity_as_extraction_mechanism, empirical, 'Whether clinician conscience burden is coordination cost or extractive transfer').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(end_of_life_authority__autonomy_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(end__tr_t0, end_of_life_authority__autonomy_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(end__tr_t6, end_of_life_authority__autonomy_reading, theater_ratio, 6, 0.07).
narrative_ontology:measurement(end__tr_t12, end_of_life_authority__autonomy_reading, theater_ratio, 12, 0.09).
narrative_ontology:measurement(end__tr_t18, end_of_life_authority__autonomy_reading, theater_ratio, 18, 0.1).
narrative_ontology:measurement(end__tr_t24, end_of_life_authority__autonomy_reading, theater_ratio, 24, 0.11).
narrative_ontology:measurement(end__tr_t30, end_of_life_authority__autonomy_reading, theater_ratio, 30, 0.12).

% Extraction over time
narrative_ontology:measurement(end__be_t0, end_of_life_authority__autonomy_reading, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(end__be_t6, end_of_life_authority__autonomy_reading, base_extractiveness, 6, 0.11).
narrative_ontology:measurement(end__be_t12, end_of_life_authority__autonomy_reading, base_extractiveness, 12, 0.14).
narrative_ontology:measurement(end__be_t18, end_of_life_authority__autonomy_reading, base_extractiveness, 18, 0.15).
narrative_ontology:measurement(end__be_t24, end_of_life_authority__autonomy_reading, base_extractiveness, 24, 0.17).
narrative_ontology:measurement(end__be_t30, end_of_life_authority__autonomy_reading, base_extractiveness, 30, 0.18).

% Suppression requirement over time
narrative_ontology:measurement(end__su_t0, end_of_life_authority__autonomy_reading, suppression_requirement, 0, 0.85).
narrative_ontology:measurement(end__su_t6, end_of_life_authority__autonomy_reading, suppression_requirement, 6, 0.8).
narrative_ontology:measurement(end__su_t12, end_of_life_authority__autonomy_reading, suppression_requirement, 12, 0.76).
narrative_ontology:measurement(end__su_t18, end_of_life_authority__autonomy_reading, suppression_requirement, 18, 0.73).
narrative_ontology:measurement(end__su_t24, end_of_life_authority__autonomy_reading, suppression_requirement, 24, 0.72).
narrative_ontology:measurement(end__su_t30, end_of_life_authority__autonomy_reading, suppression_requirement, 30, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(end_of_life_authority__autonomy_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(end_of_life_authority__autonomy_reading, 0.08).
narrative_ontology:affects_constraint(end_of_life_authority__autonomy_reading, end_of_life_authority__sanctity_reading).
narrative_ontology:affects_constraint(end_of_life_authority__autonomy_reading, end_of_life_authority__slippery_slope_mechanism).

% DUAL FORMULATION NOTE:
% The end_of_life_authority kernel decomposes into three constraint stories: autonomy_reading (this file), sanctity_reading, and slippery_slope_mechanism. The autonomy reading's ε (0.18) is substantially lower than the sanctity reading's ε would be for those it governs (prohibition extracts from all suffering patients) but higher than a pure rope due to the suffering_prolonged victim set. The slippery_slope_mechanism is not a separate constraint but a structural drift hypothesis about the autonomy reading's trajectory — it is linked via affects_constraints because the slippery_slope reading's empirical claims about expansion patterns are evidence for or against the autonomy reading's mandatrophy status.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(end_of_life_authority__autonomy_reading, moderate, 0.2).
constraint_indexing:directionality_override(end_of_life_authority__autonomy_reading, powerless, 0.85).
constraint_indexing:directionality_override(end_of_life_authority__autonomy_reading, institutional, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
