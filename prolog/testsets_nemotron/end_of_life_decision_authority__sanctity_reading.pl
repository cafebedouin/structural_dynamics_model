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
 *   human_readable: Sanctity of Life Constraint on End-of-Life Decisions
 *   domain: medical_ethics/bioethics/end_of_life_policy
 *
 * SUMMARY:
 *   The sanctity reading instantiates end-of-life authority as an absolute
 *   prohibition on intentional life-ending, grounded in the claim that human
 *   life possesses intrinsic value independent of individual will. This
 *   reading treats the physician's role as exclusively healer, never killer,
 *   and externalizes individual suffering as a burden to be borne rather than
 *   a basis for authority. When euthanasia or assisted dying becomes legally
 *   available, the reading argues that pressured-vulnerable patients
 *   (elderly, disabled, poor) enter the victim set through structural
 *   coercion — subtle family pressure, resource constraints, internalized
 *   burden narratives — that the availability of death-as-option makes
 *   unavoidable. The constraint requires active enforcement (criminal
 *   prohibition, professional discipline, institutional policy) to maintain
 *   the prohibition; its extraction is the forced continuation of life
 *   against competent will, and its suppression is the elimination of legal
 *   exit pathways.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(end_of_life_decision_authority__sanctity_reading, 0.72).
domain_priors:suppression_score(end_of_life_decision_authority__sanctity_reading, 0.85).
domain_priors:theater_ratio(end_of_life_decision_authority__sanctity_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(end_of_life_decision_authority__sanctity_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(end_of_life_decision_authority__sanctity_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(end_of_life_decision_authority__sanctity_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(end_of_life_decision_authority__sanctity_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(end_of_life_decision_authority__sanctity_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(end_of_life_decision_authority__sanctity_reading, snare).
narrative_ontology:human_readable(end_of_life_decision_authority__sanctity_reading, "Sanctity of Life Constraint on End-of-Life Decisions").
narrative_ontology:topic_domain(end_of_life_decision_authority__sanctity_reading, "medical_ethics/bioethics/end_of_life_policy").

domain_priors:requires_active_enforcement(end_of_life_decision_authority__sanctity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(end_of_life_decision_authority__sanctity_reading, '4f691815-6fb1-4ad5-95d6-4f4b6d76e6a6').
narrative_ontology:cs_kernel_codification('4f691815-6fb1-4ad5-95d6-4f4b6d76e6a6', distributed).
narrative_ontology:cs_authority_grounding('4f691815-6fb1-4ad5-95d6-4f4b6d76e6a6', extraction).
narrative_ontology:cs_reading_relation('4f691815-6fb1-4ad5-95d6-4f4b6d76e6a6', end_of_life_decision_authority__autonomy_reading, forecloses).
narrative_ontology:cs_reading_relation('4f691815-6fb1-4ad5-95d6-4f4b6d76e6a6', end_of_life_decision_authority__vulnerability_protection_reading, influences).
narrative_ontology:cs_axiom('4f691815-6fb1-4ad5-95d6-4f4b6d76e6a6', foundational, human_life_intrinsically_valuable_independent_of_will).
narrative_ontology:cs_axiom_status(human_life_intrinsically_valuable_independent_of_will, holdable).
narrative_ontology:cs_axiom_grounding('4f691815-6fb1-4ad5-95d6-4f4b6d76e6a6', human_life_intrinsically_valuable_independent_of_will, deontological).
narrative_ontology:cs_axiom('4f691815-6fb1-4ad5-95d6-4f4b6d76e6a6', foundational, intentional_killing_always_violates_intrinsic_value).
narrative_ontology:cs_axiom_status(intentional_killing_always_violates_intrinsic_value, holdable).
narrative_ontology:cs_axiom_grounding('4f691815-6fb1-4ad5-95d6-4f4b6d76e6a6', intentional_killing_always_violates_intrinsic_value, deontological).
narrative_ontology:cs_axiom('4f691815-6fb1-4ad5-95d6-4f4b6d76e6a6', secondary, physician_role_exclusively_healer_never_killer).
narrative_ontology:cs_axiom_status(physician_role_exclusively_healer_never_killer, holdable).
narrative_ontology:cs_axiom_grounding('4f691815-6fb1-4ad5-95d6-4f4b6d76e6a6', physician_role_exclusively_healer_never_killer, conventional).
narrative_ontology:cs_reference_frame('4f691815-6fb1-4ad5-95d6-4f4b6d76e6a6', absolute_prohibition_on_intentional_killing).
narrative_ontology:cs_drift_state('4f691815-6fb1-4ad5-95d6-4f4b6d76e6a6', contemporary_assisted_dying_legalization_wave, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('4f691815-6fb1-4ad5-95d6-4f4b6d76e6a6', '').
narrative_ontology:cs_kernel_id(end_of_life_decision_authority__sanctity_reading, end_of_life_decision_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(end_of_life_decision_authority__sanctity_reading, religious_institutions_upholding_sanctity_doctrine).
narrative_ontology:constraint_beneficiary(end_of_life_decision_authority__sanctity_reading, disability_rights_organizations_opposing_euthanasia).
narrative_ontology:constraint_beneficiary(end_of_life_decision_authority__sanctity_reading, palliative_care_institutions_maintaining_healer_role).
narrative_ontology:constraint_victim(end_of_life_decision_authority__sanctity_reading, terminally_ill_patients_seeking_physician_assisted_death).
narrative_ontology:constraint_victim(end_of_life_decision_authority__sanctity_reading, pressured_vulnerable_patients_facing_coercive_pressure).
narrative_ontology:constraint_victim(end_of_life_decision_authority__sanctity_reading, physicians_conscientiously_objecting_to_healer_role_violation).
narrative_ontology:constraint_vindicates(end_of_life_decision_authority__sanctity_reading, intrinsic_value_of_human_life).
narrative_ontology:constraint_vindicates(end_of_life_decision_authority__sanctity_reading, physician_role_as_healer_only).
narrative_ontology:constraint_vindicates(end_of_life_decision_authority__sanctity_reading, prohibition_on_intentional_killing).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain doctrinal coherence and moral authority through the absolute prohibition on intentional killing. The prohibition costs them nothing directly — they do not experience the forced life-continuation — but provides institutional identity, political mobilization, and claim to represent universal human dignity. Their exit from this constraint would be doctrinal revision, which is existentially costly to institutional identity.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__sanctity_reading, religious_institutions_upholding_sanctity_doctrine, beneficiary,
    institutional, civilizational, arbitrage, global).

% Organize against assisted dying laws on the grounds that they create structural pressure on disabled people to choose death. They collect moral authority and policy influence from this position. Their exit would require abandoning a core advocacy position that defines their public relevance; they are identity-locked to the vulnerability prediction.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__sanctity_reading, disability_rights_organizations_opposing_euthanasia, beneficiary,
    organized, generational, constrained, national).

% Professionally and institutionally invested in the healer-only role boundary. They set clinical norms, train clinicians, and advocate for palliative care as the alternative to assisted dying. The prohibition protects their professional jurisdiction and the coherence of palliative care as a non-lethal specialty. Exit would mean integrating assisted dying into palliative practice, which fractures their professional identity.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__sanctity_reading, palliative_care_institutions_maintaining_healer_role, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(end_of_life_decision_authority__sanctity_reading, palliative_care_institutions_maintaining_healer_role, agenda_setter).

% Competent patients with terminal illness who have determined that their suffering warrants assisted death. They bear the full extraction: forced continuation of life they have judged not worth living. Legal exit pathways are eliminated — they can refuse treatment (which may not end suffering), stop eating/drinking (prolonged, distressing), travel to permissive jurisdictions (costly, legally complex, requires capacity), or die by suicide (violent, solitary, stigmatized). They are trapped by the constraint's active suppression of the one exit they have chosen.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__sanctity_reading, terminally_ill_patients_seeking_physician_assisted_death, payer,
    moderate, immediate, trapped, local).

% Patients (elderly, disabled, poor, dependent) who do not seek death but face subtle structural pressure — family burden narratives, resource constraints, internalized worthlessness — that the availability of assisted dying would amplify. The sanctity reading claims to protect them by maintaining the prohibition, but they bear the cost of that protection: their autonomy is restricted to prevent a coercion they may not experience, and their voices are often spoken for by disability organizations. Their identity is fused with the 'vulnerable' category the reading constructs; exit from that identity is existentially costly.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__sanctity_reading, pressured_vulnerable_patients_facing_coercive_pressure, payer,
    powerless, immediate, identity_locked, local).

% Physicians who would provide assisted dying if legal but are prohibited by law and professional discipline. They bear professional constraint, moral injury from being unable to honor patient requests they judge legitimate, and the cost of maintaining a professional identity they experience as incoherent (healer role that requires watching suffering they could relieve). Their exit options are constrained: leave medicine (career destruction), practice in permissive jurisdictions (relocation), or comply silently. They are not identity-locked — many support the prohibition — but those who would provide are payers.
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__sanctity_reading, physicians_conscientiously_objecting_to_healer_role_violation, payer,
    powerful, biographical, constrained, national).

% Analyze the structural relationships between the three readings, the empirical evidence on vulnerability, the professional ethics of the healer role, and the legal architecture of end-of-life authority. They collect no rents from the prohibition and bear no bodily costs; their exit is analytical (changing framework).
narrative_ontology:constraint_stakeholder(end_of_life_decision_authority__sanctity_reading, bioethics_scholars_analyzing_end_of_life_authority, observer,
    analytical, generational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a bright-line prohibition on intentional killing that protects the healer role, prevents state-sanctioned killing from expanding beyond narrow criteria, and provides a shared moral framework that treats all human life as equally inviolable regardless of capacity or condition.
% TRANSFER_FUNCTION: Transfers the totality of remaining autonomy and chosen death from terminally ill patients seeking assisted death to the prohibition's beneficiaries (religious institutions, disability organizations, palliative care institutions) who collect moral authority, professional coherence, and policy control. Also transfers the risk of coercion from pressured-vulnerable patients (who are 'protected' by having their autonomy restricted) to the broader public who bear the cost of the prohibition's enforcement.
% ABSENT_VOICES: Terminally ill patients who died without access to assisted dying and left no record; future patients who will face the same constraint; physicians in permissive jurisdictions who have integrated assisted dying into practice and report no healer-role collapse (their experience is excluded from the sanctity reading's evidence base); families who supported a loved one's assisted death and experienced it as peaceful rather than coercive.
% DISAPPEARANCE_RATIONALE: If the sanctity prohibition vanished overnight, jurisdictions would rapidly legalize assisted dying with varying checkpoint architectures (the vulnerability reading's structure). Terminally ill patients would gain legal access; pressured-vulnerable patients would face the predicted coercion risks (requiring the vulnerability reading's safeguards); physicians would integrate assisted dying into practice (some reluctantly, some willingly); religious institutions and disability organizations would lose their central policy anchor and reorganize around new advocacy; palliative care would face professional identity crisis. The world rearranges completely.
% FOUNDING_PROBLEM: Prevent state-sanctioned killing from expanding from voluntary euthanasia to involuntary euthanasia and eugenics, as occurred in Nazi Germany and was feared in early bioethics debates; protect the physician healer role from corruption into a killer role; establish a bright-line moral prohibition that treats all human life as equally inviolable.
% FOUNDING_PROBLEM_CORROBORATION: The Nazi eugenics history is corroborated by historians outside the benefiting parties (historical fact). The slippery slope from voluntary to involuntary euthanasia is contested: Dutch and Belgian longitudinal studies (independent of the benefiting parties) show no expansion to non-voluntary euthanasia after legalization, but disability organizations and religious institutions contest the methodology and point to individual cases. The healer role corruption claim is corroborated by some physician surveys (independent) but contested by physicians in permissive jurisdictions who report integration without role collapse.
narrative_ontology:disappearance_verdict(end_of_life_decision_authority__sanctity_reading, world_rearranges).
narrative_ontology:founding_problem_status(end_of_life_decision_authority__sanctity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(end_of_life_decision_authority__sanctity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(end_of_life_decision_authority__sanctity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(end_of_life_decision_authority__sanctity_reading, 0.72, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness (0.72) reflects the magnitude of life-time forcibly continued against competent refusal — the totality of remaining autonomy extracted. Suppression (0.85) is high because the constraint eliminates legal alternatives entirely; the only exits are extralegal (suicide, treatment refusal, travel) which carry their own costs and risks. Theater ratio (0.25) is moderate-low: the healer-role justification is genuinely believed by proponents, but the constraint's persistence in jurisdictions with majority public support for assisted dying suggests the enforcement machinery serves the prohibition more than the coordination function. Accessibility collapse (0.65) reflects that alternatives (palliative sedation, treatment refusal) exist but are experienced as inadequate by those seeking assisted death. Resistance (0.45) is moderate: organized advocacy for legal change exists but has achieved limited success against the prohibition.
 *
 * PERSPECTIVAL GAP:
 *   From the beneficiary seats (religious institutions, disability orgs), the constraint is a protective Mountain — a bulwark against a slippery slope they genuinely believe would kill the vulnerable. From the target seats (terminally ill patients), it is a Snare — active suppression of a chosen exit with no recourse. From the physician seat, it is a Tangled Rope — the healer role provides genuine coordination value (trust, non-abandonment) but the absolute prohibition extracts from those who would integrate assisted dying into that role. The engine computes this per-seat divergence from the structural data; the claimed_type 'snare' reflects the target-seat experience which dominates the constraint's operational reality.
 *
 * DIRECTIONALITY LOGIC:
 *   Religious institutions and disability rights organizations are structural beneficiaries: they collect moral authority, institutional coherence, and policy influence from the prohibition without bearing its direct bodily costs. Palliative care institutions benefit by maintaining the healer-only role boundary. Terminally ill patients seeking assisted death are full targets: they bear the total extraction of forced life-continuation. Pressured-vulnerable patients are targets through a different mechanism: they bear the risk of coercion that the reading predicts but cannot individually verify or resist. Physicians who would provide assisted death but are prohibited are payers: they bear professional constraint and moral injury from being unable to honor patient requests. The directionality derivation places beneficiaries at low d (subsidized), targets at high d (extracted from), and physicians at intermediate d.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing state-sanctioned killing of the vulnerable) remains live but contested. The constraint has not resolved its mandatrophy because the vulnerability it was built to prevent (involuntary euthanasia, eugenics) is empirically distinguishable from the voluntary assisted dying it now suppresses — yet the reading treats them as identical. The mandate has outlived its precise function but persists because the beneficiary coalition treats any distinction as the slope itself.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sanctity_vs_constructed_prohibition,
    'Is the prohibition on intentional life-ending a genuine natural law (Mountain) or a constructed constraint that benefits identifiable institutions?',
    'Cross-cultural and historical analysis of end-of-life norms: if the prohibition is universal across societies without shared institutional heritage, it is more Mountain-like; if it tracks specific religious/legal traditions, it is more constructed.',
    'If constructed, the constraint is a false summit candidate (beneficiaries declared on a claimed Mountain would trigger FSM); as authored snare, this omega documents the irreducible ambiguity in the naturalness claim itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sanctity_vs_constructed_prohibition, conceptual, 'Natural law vs. institutional construction of the sanctity prohibition').

omega_variable(
    pressured_vulnerable_mechanism,
    'Does the availability of assisted dying structurally generate coercion against vulnerable populations, or does it reveal pre-existing coercion that was previously invisible?',
    'Longitudinal data from jurisdictions with legal assisted dying: measure rates of non-voluntary euthanasia, family pressure reports, and socioeconomic disparities in access before and after legalization, controlling for reporting changes.',
    'If legalization generates new coercion, the sanctity reading''s victim prediction is validated and its extractive suppression of autonomy is partly justified as protection. If it only reveals pre-existing coercion, the sanctity reading uses the vulnerable as a shield for its own prohibition.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(pressured_vulnerable_mechanism, empirical, 'Causal mechanism of vulnerability under assisted dying regimes').

omega_variable(
    healer_role_coherence,
    'Is the physician healer-only role a coherent professional identity that would collapse if assisted dying were integrated, or a contested boundary that has already absorbed other lethal acts (palliative sedation, withdrawal of life support)?',
    'Professional ethics analysis: map the boundary of ''healing'' across accepted end-of-life practices; test whether assisted dying is structurally distinct or a difference of degree.',
    'If the healer role has already absorbed lethal acts, the sanctity reading''s coordination claim (protecting professional integrity) is partially theatrical — the boundary is already permeable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(healer_role_coherence, conceptual, 'Whether the healer/killer distinction survives internal professional scrutiny').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(end_of_life_decision_authority__sanctity_reading, 1970, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(end__tr_t1970, end_of_life_decision_authority__sanctity_reading, theater_ratio, 1970, 0.15).
narrative_ontology:measurement(end__tr_t1985, end_of_life_decision_authority__sanctity_reading, theater_ratio, 1985, 0.18).
narrative_ontology:measurement(end__tr_t1995, end_of_life_decision_authority__sanctity_reading, theater_ratio, 1995, 0.2).
narrative_ontology:measurement(end__tr_t2005, end_of_life_decision_authority__sanctity_reading, theater_ratio, 2005, 0.22).
narrative_ontology:measurement(end__tr_t2015, end_of_life_decision_authority__sanctity_reading, theater_ratio, 2015, 0.24).
narrative_ontology:measurement(end__tr_t2025, end_of_life_decision_authority__sanctity_reading, theater_ratio, 2025, 0.25).

% Extraction over time
narrative_ontology:measurement(end__be_t1970, end_of_life_decision_authority__sanctity_reading, base_extractiveness, 1970, 0.45).
narrative_ontology:measurement(end__be_t1985, end_of_life_decision_authority__sanctity_reading, base_extractiveness, 1985, 0.52).
narrative_ontology:measurement(end__be_t1995, end_of_life_decision_authority__sanctity_reading, base_extractiveness, 1995, 0.6).
narrative_ontology:measurement(end__be_t2005, end_of_life_decision_authority__sanctity_reading, base_extractiveness, 2005, 0.68).
narrative_ontology:measurement(end__be_t2015, end_of_life_decision_authority__sanctity_reading, base_extractiveness, 2015, 0.7).
narrative_ontology:measurement(end__be_t2025, end_of_life_decision_authority__sanctity_reading, base_extractiveness, 2025, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(end__su_t1970, end_of_life_decision_authority__sanctity_reading, suppression_requirement, 1970, 0.7).
narrative_ontology:measurement(end__su_t1985, end_of_life_decision_authority__sanctity_reading, suppression_requirement, 1985, 0.75).
narrative_ontology:measurement(end__su_t1995, end_of_life_decision_authority__sanctity_reading, suppression_requirement, 1995, 0.8).
narrative_ontology:measurement(end__su_t2005, end_of_life_decision_authority__sanctity_reading, suppression_requirement, 2005, 0.82).
narrative_ontology:measurement(end__su_t2015, end_of_life_decision_authority__sanctity_reading, suppression_requirement, 2015, 0.84).
narrative_ontology:measurement(end__su_t2025, end_of_life_decision_authority__sanctity_reading, suppression_requirement, 2025, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(end_of_life_decision_authority__sanctity_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(end_of_life_decision_authority__sanctity_reading, 0.08).
narrative_ontology:affects_constraint(end_of_life_decision_authority__sanctity_reading, end_of_life_decision_authority__autonomy_reading).
narrative_ontology:affects_constraint(end_of_life_decision_authority__sanctity_reading, end_of_life_decision_authority__vulnerability_protection_reading).

% DUAL FORMULATION NOTE:
% This constraint is the sanctity_reading of the end_of_life_decision_authority kernel. It decomposes the kernel's three structurally distinct claims: the autonomy reading (ε≈0.15, low extraction, high coordination for competent agents), the sanctity reading (ε≈0.72, high extraction from those seeking death, active suppression of alternatives), and the vulnerability protection reading (ε≈0.40, moderate extraction from both denial and coercion sides, checkpoint architecture as coordination). The sanctity reading's ε is substantially higher because it extracts the totality of remaining autonomy from terminally ill patients, while the autonomy reading extracts near-zero from those who do not want assisted dying (they simply don't use it). The vulnerability reading's checkpoint architecture distributes extraction across multiple institutional seats.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(end_of_life_decision_authority__sanctity_reading, institutional, 0.1).
constraint_indexing:directionality_override(end_of_life_decision_authority__sanctity_reading, organized, 0.15).
constraint_indexing:directionality_override(end_of_life_decision_authority__sanctity_reading, moderate, 0.85).
constraint_indexing:directionality_override(end_of_life_decision_authority__sanctity_reading, powerful, 0.9).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
