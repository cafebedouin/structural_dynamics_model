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
 *   constraint_id: dignified_death__sanctity_primary
 *   human_readable: Sanctity of Life as Absolute Prohibition on Assisted Dying
 *   domain: bioethics/medical_law/political_philosophy
 *
 * SUMMARY:
 *   This constraint story instantiates the sanctity_primary reading of the
 *   dignified_death kernel: dignity is grounded in life's intrinsic value,
 *   and intentional life-termination violates transcendent moral law
 *   regardless of consent. In jurisdictions where this reading dominates, the
 *   prohibition is presented as absolute moral truth, yet it generates
 *   identifiable victim populationsâelderly, disabled, and impoverished
 *   patients who are coerced into prolonged suffering. The constraint is
 *   claimed by its defenders as a natural moral boundary (Mountain-like in
 *   its absolutism), but the structural presence of beneficiaries
 *   (sanctity-preserving institutions) and victims (vulnerable patients)
 *   places it in the snare category: a protection norm that has become
 *   extractive through coerced prolongation of life. This is one reading of a
 *   three-way kernel; the autonomy_primary and relational_autonomy readings
 *   are instantiated as separate constraints.
 *
 * KEY AGENTS:
 *   - Sanctity coalition (beneficiary): Religious, cultural, and legal institutions whose authority depends on the absolute inviolability of life; they collect deference and institutional legitimacy.
 *   - Prohibition-enforcing state (agenda-setter): State apparatus that criminalizes assisted dying and polices medical practice to maintain the absolute prohibition.
 *   - Elderly, disabled, impoverished patients (payer/victim): Terminally ill and vulnerable individuals legally barred from assisted dying who bear the cost of prolonged suffering and lost bodily autonomy.
 *   - Medical practitioners (payer): Clinicians compelled to continue life-sustaining treatment against patient wishes, bearing moral distress and legal jeopardy.
 *   - Autonomy advocates (excluded): Patient-rights and disability-justice advocates structurally excluded from institutional ethics frameworks where sanctity is non-negotiable.
 *   - Bioethics observer (analytical): Scholars analyzing the structural asymmetry between the prohibition's protective claims and its coercive effects.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dignified_death__sanctity_primary, 0.62).
domain_priors:suppression_score(dignified_death__sanctity_primary, 0.58).
domain_priors:theater_ratio(dignified_death__sanctity_primary, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dignified_death__sanctity_primary, extractiveness, 0.62).
narrative_ontology:constraint_metric(dignified_death__sanctity_primary, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(dignified_death__sanctity_primary, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dignified_death__sanctity_primary, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(dignified_death__sanctity_primary, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dignified_death__sanctity_primary, snare).
narrative_ontology:human_readable(dignified_death__sanctity_primary, "Sanctity of Life as Absolute Prohibition on Assisted Dying").
narrative_ontology:topic_domain(dignified_death__sanctity_primary, "bioethics/medical_law/political_philosophy").

domain_priors:requires_active_enforcement(dignified_death__sanctity_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dignified_death__sanctity_primary, 'a7aa13ba-f7d4-42fa-b863-0fed3afc9760').
narrative_ontology:cs_kernel_codification('a7aa13ba-f7d4-42fa-b863-0fed3afc9760', fixed_text).
narrative_ontology:cs_authority_grounding('a7aa13ba-f7d4-42fa-b863-0fed3afc9760', lineage).
narrative_ontology:cs_interpretation_layer_present('a7aa13ba-f7d4-42fa-b863-0fed3afc9760').
narrative_ontology:cs_reading_relation('a7aa13ba-f7d4-42fa-b863-0fed3afc9760', dignified_death__autonomy_primary, forecloses).
narrative_ontology:cs_reading_relation('a7aa13ba-f7d4-42fa-b863-0fed3afc9760', dignified_death__relational_autonomy, forecloses).
narrative_ontology:cs_axiom('a7aa13ba-f7d4-42fa-b863-0fed3afc9760', foundational, intrinsic_dignity_forbids_intentional_termination).
narrative_ontology:cs_axiom_status(intrinsic_dignity_forbids_intentional_termination, holdable).
narrative_ontology:cs_axiom_grounding('a7aa13ba-f7d4-42fa-b863-0fed3afc9760', intrinsic_dignity_forbids_intentional_termination, deontological).
narrative_ontology:cs_axiom('a7aa13ba-f7d4-42fa-b863-0fed3afc9760', foundational, patient_autonomy_cannot_justify_life_termination).
narrative_ontology:cs_axiom_status(patient_autonomy_cannot_justify_life_termination, holdable).
narrative_ontology:cs_axiom_grounding('a7aa13ba-f7d4-42fa-b863-0fed3afc9760', patient_autonomy_cannot_justify_life_termination, deontological).
narrative_ontology:cs_reference_frame('a7aa13ba-f7d4-42fa-b863-0fed3afc9760', absolute_sanctity_framework).
narrative_ontology:cs_drift_state('a7aa13ba-f7d4-42fa-b863-0fed3afc9760', contemporary_secular_bioethics_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('a7aa13ba-f7d4-42fa-b863-0fed3afc9760', '').
narrative_ontology:cs_kernel_id(dignified_death__sanctity_primary, dignified_death).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dignified_death__sanctity_primary, sanctity_coalition).
narrative_ontology:constraint_victim(dignified_death__sanctity_primary, elderly_disabled_impoverished_patients).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(dignified_death__sanctity_primary, medical_practitioners).
narrative_ontology:constraint_vindicates(dignified_death__sanctity_primary, sanctity_of_life_doctrine).
narrative_ontology:constraint_vindicates(dignified_death__sanctity_primary, absolute_prohibition_norm).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Religious, cultural, and legal institutions whose authority and social legitimacy depend on maintaining the absolute inviolability of human life. They collect deference, institutional role, and political influence from the prohibition's persistence and from the framing of assisted dying as moral corruption.
narrative_ontology:constraint_stakeholder(dignified_death__sanctity_primary, sanctity_coalition, beneficiary,
    institutional, generational, constrained, global).

% State apparatus that criminalizes assisted suicide and euthanasia through penal codes and medical licensing laws, enforcing the transcendent moral prohibition via prosecution, professional discipline, and policing of clinical practice.
narrative_ontology:constraint_stakeholder(dignified_death__sanctity_primary, prohibition_enforcing_state, agenda_setter,
    institutional, generational, constrained, national).

% Terminally ill, elderly, disabled, and impoverished individuals who are legally barred from accessing assisted dying and may face implicit or explicit pressure to persist in treatment. They bear the direct cost of prolonged suffering, loss of bodily autonomy, and medicalization of death.
narrative_ontology:constraint_stakeholder(dignified_death__sanctity_primary, elderly_disabled_impoverished_patients, payer,
    powerless, immediate, trapped, national).

% Physicians and clinicians who are legally compelled to continue life-sustaining treatment even when patients refuse or suffer unbearably, bearing moral distress, legal jeopardy, and professional risk if they honor patient requests to die.
narrative_ontology:constraint_stakeholder(dignified_death__sanctity_primary, medical_practitioners, payer,
    moderate, biographical, constrained, national).

% Patient-rights and disability-justice advocates who argue for self-determination in end-of-life decisions. They are present in public discourse but structurally excluded from legislative and institutional ethics committees where sanctity is treated as non-negotiable.
narrative_ontology:constraint_stakeholder(dignified_death__sanctity_primary, autonomy_advocates, excluded,
    organized, generational, constrained, national).

% Academic bioethicists and comparative legal scholars who analyze the structural asymmetry between the prohibition's protective claims and its coercive effects on vulnerable populations.
narrative_ontology:constraint_stakeholder(dignified_death__sanctity_primary, bioethics_observer, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(dignified_death__sanctity_primary, sanctity_coalition).
narrative_ontology:fixing_cost_class(dignified_death__sanctity_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Claimed coordination: protects vulnerable populations from familial, social, and economic pressure to end their lives by maintaining an absolute, non-waivable prohibition on intentional life-termination, thereby preserving social trust in medicine and preventing utilitarian disposal of the powerless.
% TRANSFER_FUNCTION: Moves the authority to determine the timing and manner of death from the individual patient to the state and moral institutions; transfers the cost of prolonged existence and medicalized suffering onto vulnerable patients and clinicians while moral institutions collect deference, legitimacy, and institutional role.
% ABSENT_VOICES: Terminally ill patients who desire death but are legally and medically silenced; impoverished patients who cannot afford palliative care and are trapped in prolonged suffering; and disability-rights advocates who view the prohibition as paternalistic but are excluded from institutional ethics frameworks that treat sanctity as non-negotiable.
% DISAPPEARANCE_RATIONALE: If the absolute prohibition vanished, jurisdictions would permit regulated assisted dying regimes, medical practice would shift from mandatory life-prolongation to patient-centered goals, vulnerable patients would gain exit from coerced suffering, and the moral authority of sanctity-based institutions would diminish substantially.
% FOUNDING_PROBLEM: Protecting vulnerable individualsâespecially those with disabilities, mental illness, or limited social supportâfrom societal and familial pressure to end their lives for convenience, inheritance, or cost-saving.
% FOUNDING_PROBLEM_CORROBORATION: Disability-rights organizations outside the sanctity coalition attest the problem is live, citing fear of abuse. Patient-autonomy advocates and empirical studies from jurisdictions with assisted dying attest the problem is manageable with safeguards and that the prohibition now creates more harm than it prevents. No neutral consensus exists; corroboration is split across contested seats.
narrative_ontology:disappearance_verdict(dignified_death__sanctity_primary, world_rearranges).
narrative_ontology:founding_problem_status(dignified_death__sanctity_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dignified_death__sanctity_primary, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(dignified_death__sanctity_primary, 'none', 1).
narrative_ontology:epsilon_provenance(dignified_death__sanctity_primary, 0.62, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness (0.62) is high because the constraint forces patients to endure suffering they would otherwise escape, extracting bodily autonomy and transferring it to institutional moral authority. Suppression (0.58) reflects the active legal and medical enforcement required to maintain the prohibition against growing autonomy movements. Theater ratio (0.42) captures the increasing gap between the proclaimed protective function and the actual experience of vulnerable patients as medical technology prolongs life without restoring agency. Accessibility collapse (0.70) is high because once the sanctity frame is accepted, alternatives (assisted dying) are morally unthinkable. Resistance (0.60) is substantial from patient-rights movements and jurisdictions that have legalized autonomy. The measurement series share one time grid so every metric is authored at every examined time point.
 *
 * PERSPECTIVAL GAP:
 *   The sanctity coalition and prohibition-enforcing state experience the constraint as necessary moral protection (low directionality, perhaps even negative extractionâthey gain legitimacy and role). Vulnerable patients experience it as direct bodily extraction (high directionality, trapped exit amplifies effective extraction). Medical practitioners occupy an intermediate position: they suffer moral distress and legal coercion but also derive clarity of professional boundaries. The engine computes this divergence from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   The sanctity coalition is the structural beneficiary (gains moral authority, political deference, institutional roleâd near 0.0). The prohibition-enforcing state is agenda-setter/administrator (d near 0.15, constrained by political costs but not bearing the direct extraction). Vulnerable patients are the clear victims (d near 1.0, trapped exit amplifies effective extraction). Medical practitioners are secondary payers (d ~0.75, constrained exit, moderate power). Autonomy advocates are excluded (no d, not in beneficiary/victim derivation).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâprotecting vulnerable people from coercion into deathâis genuinely important. However, the mandate has atrophied into a structure that prolongs suffering regardless of patient consent. The classification as snare prevents mislabeling this as merely a Mountain (natural law) or Rope (coordination). It captures that the coordination story (protection) has become cover for extraction (coerced survival), and the persistence of the constraint depends on suppressing the alternative (legalized assisted dying). The divergence between the claimed protective function and the actual coercive effect is exactly what the snare category measures.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_morality,
    'Is the prohibition on intentional life-termination a transcendent moral law independent of human construction, or a socially constructed norm that benefits specific institutional actors?',
    'Cross-cultural and historical comparative analysis: if the constraint varies substantially across cultures and epochs while claiming universality, this supports constructed status.',
    'If constructed, the constraint is a false summit mountain or snare rather than a natural law; directionality shifts from universal to institutional beneficiary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_morality, conceptual, 'Ambiguity between transcendent moral law and constructed social norm.').

omega_variable(
    coercion_vs_protection,
    'Does the prohibition structurally protect vulnerable populations from coercion, or does it coerce them into prolonged suffering?',
    'Empirical study of patient-reported outcomes in jurisdictions with and without assisted dying, controlling for socioeconomic status and disability.',
    'If evidence shows net protective effect for vulnerable groups, the constraint may be a tangled_rope rather than a snare; if net coercive, snare classification strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coercion_vs_protection, empirical, 'Whether the constraint protects or harms the populations it claims to shield.').

omega_variable(
    suppression_internalized,
    'Is the suppression of assisted dying choices maintained primarily by legal enforcement, or by internalized beliefs about the sinfulness or immorality of suicide among patients and families?',
    'Attitudinal surveys and qualitative interviews with terminally ill patients in prohibition jurisdictions about their decision-making constraints.',
    'If internalized, effective extraction is higher than structural measures suggest because the target carries the suppression internally; this would increase directionality toward the target end for vulnerable patients.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_internalized, empirical, 'Structural versus internalized suppression mechanism.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dignified_death__sanctity_primary, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dds_tr_t0, dignified_death__sanctity_primary, theater_ratio, 0, 0.25).
narrative_ontology:measurement(dds_tr_t10, dignified_death__sanctity_primary, theater_ratio, 10, 0.3).
narrative_ontology:measurement(dds_tr_t20, dignified_death__sanctity_primary, theater_ratio, 20, 0.35).
narrative_ontology:measurement(dds_tr_t30, dignified_death__sanctity_primary, theater_ratio, 30, 0.4).
narrative_ontology:measurement(dds_tr_t40, dignified_death__sanctity_primary, theater_ratio, 40, 0.42).

% Extraction over time
narrative_ontology:measurement(dds_be_t0, dignified_death__sanctity_primary, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(dds_be_t10, dignified_death__sanctity_primary, base_extractiveness, 10, 0.5).
narrative_ontology:measurement(dds_be_t20, dignified_death__sanctity_primary, base_extractiveness, 20, 0.55).
narrative_ontology:measurement(dds_be_t30, dignified_death__sanctity_primary, base_extractiveness, 30, 0.6).
narrative_ontology:measurement(dds_be_t40, dignified_death__sanctity_primary, base_extractiveness, 40, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(dds_su_t0, dignified_death__sanctity_primary, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(dds_su_t10, dignified_death__sanctity_primary, suppression_requirement, 10, 0.45).
narrative_ontology:measurement(dds_su_t20, dignified_death__sanctity_primary, suppression_requirement, 20, 0.5).
narrative_ontology:measurement(dds_su_t30, dignified_death__sanctity_primary, suppression_requirement, 30, 0.55).
narrative_ontology:measurement(dds_su_t40, dignified_death__sanctity_primary, suppression_requirement, 40, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(dignified_death__sanctity_primary, dignified_death__autonomy_primary).
narrative_ontology:affects_constraint(dignified_death__sanctity_primary, dignified_death__relational_autonomy).

% DUAL FORMULATION NOTE:
% The dignified_death kernel decomposes into three structurally distinct readings (sanctity_primary, autonomy_primary, relational_autonomy) with different epsilon values, beneficiary/victim structures, and classification types. This decomposition follows the epsilon-invariance principle: the natural-language label 'dignified death' conflates multiple constraints that differ in empirical status and structural extraction.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
