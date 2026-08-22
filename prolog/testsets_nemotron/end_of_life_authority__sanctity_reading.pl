% ============================================================================
% CONSTRAINT STORY: end_of_life_authority__sanctity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_end_of_life_authority__sanctity_reading, []).

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
 *   constraint_id: end_of_life_authority__sanctity_reading
 *   human_readable: Sanctity-Based Prohibition on Assisted Dying
 *   domain: medical_ethics/bioethics/end_of_life_policy
 *
 * SUMMARY:
 *   The sanctity reading of end-of-life authority instantiates a categorical
 *   prohibition on intentional life-ending, grounded in the intrinsic value
 *   of human life. This reading operates as the default legal and
 *   professional framework in most jurisdictions: physicians are prohibited
 *   from assisting death, patients have no legal right to request it, and the
 *   healthcare system is organized around life preservation as a
 *   non-negotiable goal. The reading coordinates a genuine function —
 *   protecting vulnerable populations from coercion and preventing the
 *   normalization of killing as care — while simultaneously extracting from
 *   terminal patients who experience unbearable suffering and would choose a
 *   different outcome. The pressured-vulnerable populations (elderly,
 *   disabled, economically disadvantaged) are declared as victims because the
 *   constraint's enforcement machinery (legal prohibition, professional
 *   discipline, institutional policy) falls on them asymmetrically: they bear
 *   the costs of continued suffering when they would prefer death, while the
 *   beneficiaries (physician guardians, ethics committees, religious
 *   networks) collect the moral authority and institutional coherence the
 *   prohibition provides.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(end_of_life_authority__sanctity_reading, 0.68).
domain_priors:suppression_score(end_of_life_authority__sanctity_reading, 0.76).
domain_priors:theater_ratio(end_of_life_authority__sanctity_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(end_of_life_authority__sanctity_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(end_of_life_authority__sanctity_reading, suppression_requirement, 0.76).
narrative_ontology:constraint_metric(end_of_life_authority__sanctity_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(end_of_life_authority__sanctity_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(end_of_life_authority__sanctity_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(end_of_life_authority__sanctity_reading, tangled_rope).
narrative_ontology:human_readable(end_of_life_authority__sanctity_reading, "Sanctity-Based Prohibition on Assisted Dying").
narrative_ontology:topic_domain(end_of_life_authority__sanctity_reading, "medical_ethics/bioethics/end_of_life_policy").

domain_priors:requires_active_enforcement(end_of_life_authority__sanctity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(end_of_life_authority__sanctity_reading, 'eb8eb694-0e15-4722-a9cd-e6cc0d7b61af').
narrative_ontology:cs_kernel_codification('eb8eb694-0e15-4722-a9cd-e6cc0d7b61af', formalized).
narrative_ontology:cs_authority_grounding('eb8eb694-0e15-4722-a9cd-e6cc0d7b61af', lineage).
narrative_ontology:cs_interpretation_layer_present('eb8eb694-0e15-4722-a9cd-e6cc0d7b61af').
narrative_ontology:cs_reading_relation('eb8eb694-0e15-4722-a9cd-e6cc0d7b61af', end_of_life_authority__autonomy_reading, forecloses).
narrative_ontology:cs_reading_relation('eb8eb694-0e15-4722-a9cd-e6cc0d7b61af', end_of_life_authority__slippery_slope_mechanism, coexists_with).
narrative_ontology:cs_axiom('eb8eb694-0e15-4722-a9cd-e6cc0d7b61af', foundational, sanctity_of_life_as_intrinsic_value).
narrative_ontology:cs_axiom_status(sanctity_of_life_as_intrinsic_value, holdable).
narrative_ontology:cs_axiom_grounding('eb8eb694-0e15-4722-a9cd-e6cc0d7b61af', sanctity_of_life_as_intrinsic_value, deontological).
narrative_ontology:cs_axiom('eb8eb694-0e15-4722-a9cd-e6cc0d7b61af', foundational, physician_role_as_life_preserver).
narrative_ontology:cs_axiom_status(physician_role_as_life_preserver, holdable).
narrative_ontology:cs_axiom_grounding('eb8eb694-0e15-4722-a9cd-e6cc0d7b61af', physician_role_as_life_preserver, deontological).
narrative_ontology:cs_axiom('eb8eb694-0e15-4722-a9cd-e6cc0d7b61af', secondary, killing_versus_allowing_to_die_distinction).
narrative_ontology:cs_axiom_status(killing_versus_allowing_to_die_distinction, holdable).
narrative_ontology:cs_axiom_grounding('eb8eb694-0e15-4722-a9cd-e6cc0d7b61af', killing_versus_allowing_to_die_distinction, deontological).
narrative_ontology:cs_reference_frame('eb8eb694-0e15-4722-a9cd-e6cc0d7b61af', hippocratic_life_preservation_tradition).
narrative_ontology:cs_drift_state('eb8eb694-0e15-4722-a9cd-e6cc0d7b61af', contemporary_bioethics_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('eb8eb694-0e15-4722-a9cd-e6cc0d7b61af', '2026-08-15T14:32:00Z').
narrative_ontology:cs_kernel_id(end_of_life_authority__sanctity_reading, end_of_life_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(end_of_life_authority__sanctity_reading, physician_guardians).
narrative_ontology:constraint_beneficiary(end_of_life_authority__sanctity_reading, institutional_ethics_committees).
narrative_ontology:constraint_beneficiary(end_of_life_authority__sanctity_reading, religious_healthcare_networks).
narrative_ontology:constraint_beneficiary(end_of_life_authority__sanctity_reading, disability_rights_organizations).
narrative_ontology:constraint_victim(end_of_life_authority__sanctity_reading, terminal_patients_seeking_aid).
narrative_ontology:constraint_victim(end_of_life_authority__sanctity_reading, pressured_vulnerable_populations).
narrative_ontology:constraint_victim(end_of_life_authority__sanctity_reading, autonomy_claimants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(end_of_life_authority__sanctity_reading, pressured_vulnerable_populations).
narrative_ontology:constraint_victim(end_of_life_authority__sanctity_reading, disability_rights_organizations).
narrative_ontology:constraint_vindicates(end_of_life_authority__sanctity_reading, sanctity_of_human_life).
narrative_ontology:constraint_vindicates(end_of_life_authority__sanctity_reading, non_maleficence_principle).
narrative_ontology:constraint_vindicates(end_of_life_authority__sanctity_reading, physician_as_life_preserver).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Define and enforce the professional boundary: physicians preserve life, do not end it. They hold license authority, set medical standards, and testify in policy debates. The prohibition protects their role coherence and shields them from moral injury of participating in killing. They can move between jurisdictions or specialties if the constraint changes.
narrative_ontology:constraint_stakeholder(end_of_life_authority__sanctity_reading, physician_guardians, agenda_setter,
    institutional, generational, arbitrage, national).

% Adjudicate end-of-life cases under the sanctity framework. Their authority derives from applying the categorical prohibition. They collect institutional legitimacy and decision-making power. If the constraint changed, their role would transform but not disappear.
narrative_ontology:constraint_stakeholder(end_of_life_authority__sanctity_reading, institutional_ethics_committees, agenda_setter,
    institutional, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(end_of_life_authority__sanctity_reading, institutional_ethics_committees, beneficiary).

% Operate hospitals and care systems under doctrinal commitments that prohibit assisted dying. The legal prohibition aligns secular law with their religious identity. Their identity is fused with the constraint — changing it would require doctrinal revision, not just policy change.
narrative_ontology:constraint_stakeholder(end_of_life_authority__sanctity_reading, religious_healthcare_networks, beneficiary,
    organized, civilizational, identity_locked, global).

% Advocate for the prohibition as protection against coercion and devaluation of disabled lives. They benefit from the constraint's shield function. However, some disabled individuals experience the constraint as denying them autonomy — the organization's position may not match every constituent's interest. Exit from the organizational position is constrained by coalition politics.
narrative_ontology:constraint_stakeholder(end_of_life_authority__sanctity_reading, disability_rights_organizations, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(end_of_life_authority__sanctity_reading, disability_rights_organizations, payer).

% Face terminal illness with unbearable suffering and would choose assisted dying if legal. They bear the full cost of the constraint: prolonged suffering, loss of control, dependence on others. No legal exit exists in prohibition jurisdictions; travel to permissive jurisdictions requires resources and capacity they often lack. Their situation is the extraction referent.
narrative_ontology:constraint_stakeholder(end_of_life_authority__sanctity_reading, terminal_patients_seeking_aid, payer,
    powerless, immediate, trapped, local).

% Elderly, disabled, and economically disadvantaged people who may face subtle or overt pressure to choose death if it were an option. The constraint protects them from that pressure (beneficiary). Simultaneously, those among them who WOULD choose death are denied it (payer). Their identity as 'vulnerable' is constituted by the constraint — exit from that identity frame is cognitively and socially difficult.
narrative_ontology:constraint_stakeholder(end_of_life_authority__sanctity_reading, pressured_vulnerable_populations, payer,
    powerless, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(end_of_life_authority__sanctity_reading, pressured_vulnerable_populations, beneficiary).

% Advocate for legal assisted dying frameworks. They include bioethicists, civil liberties organizations, and some patient advocacy groups. They bear the cost of the constraint's suppression: their preferred policy is blocked, their arguments are marginalized in professional discourse. They have constrained exit — they can work for legal change but face high barriers.
narrative_ontology:constraint_stakeholder(end_of_life_authority__sanctity_reading, autonomy_claimants, payer,
    moderate, biographical, constrained, national).

% Provide symptom management within the prohibition framework. Some support the constraint (it protects palliative care's distinct identity from assisted dying); some argue it limits their ability to relieve suffering fully. They observe the constraint's operation from a clinical seat without setting its agenda.
narrative_ontology:constraint_stakeholder(end_of_life_authority__sanctity_reading, palliative_care_specialists, observer,
    organized, generational, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(end_of_life_authority__sanctity_reading, diffuse).
narrative_ontology:fixing_cost_class(end_of_life_authority__sanctity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents the normalization of killing as a medical act; protects vulnerable populations from coercion toward death; maintains professional integrity of medicine as a life-preserving enterprise; provides a clear legal-moral boundary that prevents case-by-case erosion.
% TRANSFER_FUNCTION: Transfers the burden of continued suffering from the healthcare system/society onto terminal patients who would choose death; transfers moral authority and professional coherence to physician guardians and institutional ethics committees; transfers legal risk from the state to individual physicians who might otherwise face pressure to assist dying.
% ABSENT_VOICES: Terminal patients who cannot advocate (cognitive impairment, communication barriers); future patients who will face extended dying processes under advancing medical technology; healthcare workers in jurisdictions where assisted dying is legal who report moral distress from participation — their experience is excluded from the sanctity framework's evidence base.
% DISAPPEARANCE_RATIONALE: If the categorical prohibition vanished overnight, jurisdictions would rapidly implement assisted dying frameworks (as seen in Canada, Netherlands, Oregon). Physician roles would be redefined, legal liability structures would shift, palliative care would face identity pressure, and vulnerable populations would lose their primary legal shield against coercion. The world would rearrange substantially.
% FOUNDING_PROBLEM: Preventing the abuses of state-sanctioned killing (eugenics programs, involuntary euthanasia) that demonstrated how quickly 'choice' frameworks become coercion frameworks for vulnerable populations; protecting the medical profession's identity as healers rather than killers; establishing a clear legal boundary that prevents case-by-case erosion of the prohibition on killing.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by disability rights organizations (Not Dead Yet, TASH), many religious traditions, and the World Medical Association — sources outside the physician guardian beneficiary set. However, autonomy advocates and jurisdictions with legal assisted dying (Oregon, Netherlands, Canada) attest that the founding problem is substantially solved by robust safeguards, and that the constraint now persists as extraction from terminal patients. No neutral arbiter exists; the corroboration is contested.
narrative_ontology:disappearance_verdict(end_of_life_authority__sanctity_reading, world_rearranges).
narrative_ontology:founding_problem_status(end_of_life_authority__sanctity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(end_of_life_authority__sanctity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(end_of_life_authority__sanctity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(end_of_life_authority__sanctity_reading, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(end_of_life_authority__sanctity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(end_of_life_authority__sanctity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(end_of_life_authority__sanctity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.68) reflects the substantial transfer of suffering-time from terminal patients to the constraint's maintenance: patients who would choose assisted dying are denied it, and the constraint's enforcement requires active legal and professional machinery. Suppression (0.76) is high because alternatives (assisted dying, voluntary euthanasia) are legally foreclosed and professionally sanctioned — exit is blocked by law and license. Theater ratio (0.22) is moderate-low: the protection function for vulnerable populations is real and not merely performative, but a growing share of enforcement energy defends the categorical line rather than addressing individual cases. Accessibility collapse (0.71) is high: once the sanctity principle is accepted, alternative frameworks (autonomy-based, quality-of-life) become difficult to articulate within the same moral vocabulary. Resistance (0.58) is substantial: autonomy claimants, some bioethicists, and jurisdictions that have legalized assisted dying constitute active resistance.
 *
 * PERSPECTIVAL GAP:
 *   The physician guardian seat experiences this as a Rope (genuine coordination: protects patients, structures professional identity, prevents moral injury). The terminal patient seat experiences it as a Snare (pure extraction: suffering is prolonged, exit is blocked, no benefit received). The pressured-vulnerable seat is the pivot: if the constraint protects them from coercion, it is a Rope for them; if it traps them in unwanted suffering, it is a Snare. The engine computes this divergence from the structural data — the authored claim (tangled_rope) reflects the hybrid reality that the constraint IS both coordination and extraction simultaneously.
 *
 * DIRECTIONALITY LOGIC:
 *   Physician guardians and institutional ethics committees are structural beneficiaries (d near 0.15): they hold moral authority, professional coherence, and legal protection under the prohibition. Religious healthcare networks benefit from doctrinal alignment and institutional identity (d ~0.2). Disability rights organizations occupy a complex position: they benefit from the protection function but may also be constrained by the same categorical logic (secondary_role could be payer). Terminal patients seeking aid are full targets (d ~0.85): they bear the suffering the constraint forbids relieving, with trapped exit (no legal option, professional gatekeepers block access). Pressured-vulnerable populations are identity_locked targets (d ~0.75): their vulnerability is constituted through the constraint itself — the prohibition defines them as protectable, making exit from that identity frame cognitively and socially difficult. Autonomy claimants are constrained payers (d ~0.65): they advocate for a competing framework but operate within the constraint's suppression.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing euthanasia abuses, protecting vulnerable populations from state-sanctioned killing) remains live but contested. The constraint's mandate has NOT atrophied — the protection function is still cited as necessary by disability rights groups and many physicians. However, the extraction component has accumulated: as medical technology extends dying processes, the suffering denied relief grows, and the constraint's coordination function (protection) covers a shrinking proportion of the cases it governs. This is a classic Tangled Rope: the coordination function is real but the extraction has grown disproportionate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_framing,
    'Is the categorical prohibition on assisted dying a genuine natural law constraint (Mountain) or a constructed institutional constraint that benefits identifiable actors (Tangled Rope/Snare)?',
    'Cross-jurisdictional comparison of patient outcomes and physician role definitions in regimes with and without assisted dying laws; structural analysis of who materially benefits from the prohibition''s enforcement.',
    'If natural law, the constraint is a Mountain with negligible extraction; if constructed, the beneficiary structure and extraction profile are real and the constraint is a Tangled Rope (coordination + asymmetric extraction) or Snare (pure extraction). This reading''s claimed_type is contested at the meta-level.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_framing, conceptual, 'Whether the sanctity reading instantiates a natural law or a constructed institutional constraint.').

omega_variable(
    coercion_measurement_ambiguity,
    'How much of the measured suppression is structural (legal barriers, professional sanctions) versus internalized (patients who believe requesting death is morally wrong, families who would never consider it)?',
    'Longitudinal study of patient attitudes and request rates in jurisdictions that legalize assisted dying: if request rates remain low after legalization, internalized suppression is high; if rates rise sharply, structural suppression was the dominant mechanism.',
    'If internalized suppression is substantial, the constraint''s effective suppression exceeds the legal/structural measure — the target carries the prohibition internally after structural barriers are removed. This affects both the victim set definition and the extraction calculation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coercion_measurement_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in interpersonal/institutional end-of-life contexts.').

omega_variable(
    victim_set_boundary,
    'Does the pressured-vulnerable victim set (elderly, disabled, economically disadvantaged) experience this constraint as extraction (coercion to live) or as protection (coercion against premature death)?',
    'Revealed preference and stated preference studies of vulnerable populations in jurisdictions with and without assisted dying; analysis of safeguard effectiveness and abuse rates in existing regimes.',
    'If protection, the victim declaration is wrong — the constraint coordinates a genuine protection function. If extraction, the victim set is real and the constraint''s asymmetric extraction is confirmed. The ambiguity is structural: the same constraint can be protection for some vulnerable people and extraction for others simultaneously.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_set_boundary, preference, 'Whether pressured-vulnerable populations are victims of extraction or beneficiaries of protection under the sanctity reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(end_of_life_authority__sanctity_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eol_sanctity_tr_t0, end_of_life_authority__sanctity_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(eol_sanctity_tr_t10, end_of_life_authority__sanctity_reading, theater_ratio, 10, 0.11).
narrative_ontology:measurement(eol_sanctity_tr_t20, end_of_life_authority__sanctity_reading, theater_ratio, 20, 0.15).
narrative_ontology:measurement(eol_sanctity_tr_t30, end_of_life_authority__sanctity_reading, theater_ratio, 30, 0.19).
narrative_ontology:measurement(eol_sanctity_tr_t40, end_of_life_authority__sanctity_reading, theater_ratio, 40, 0.22).

% Extraction over time
narrative_ontology:measurement(eol_sanctity_be_t0, end_of_life_authority__sanctity_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(eol_sanctity_be_t10, end_of_life_authority__sanctity_reading, base_extractiveness, 10, 0.51).
narrative_ontology:measurement(eol_sanctity_be_t20, end_of_life_authority__sanctity_reading, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(eol_sanctity_be_t30, end_of_life_authority__sanctity_reading, base_extractiveness, 30, 0.63).
narrative_ontology:measurement(eol_sanctity_be_t40, end_of_life_authority__sanctity_reading, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(eol_sanctity_su_t0, end_of_life_authority__sanctity_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(eol_sanctity_su_t10, end_of_life_authority__sanctity_reading, suppression_requirement, 10, 0.61).
narrative_ontology:measurement(eol_sanctity_su_t20, end_of_life_authority__sanctity_reading, suppression_requirement, 20, 0.68).
narrative_ontology:measurement(eol_sanctity_su_t30, end_of_life_authority__sanctity_reading, suppression_requirement, 30, 0.72).
narrative_ontology:measurement(eol_sanctity_su_t40, end_of_life_authority__sanctity_reading, suppression_requirement, 40, 0.76).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(end_of_life_authority__sanctity_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(end_of_life_authority__sanctity_reading, 0.12).
narrative_ontology:affects_constraint(end_of_life_authority__sanctity_reading, end_of_life_authority__autonomy_reading).
narrative_ontology:affects_constraint(end_of_life_authority__sanctity_reading, end_of_life_authority__slippery_slope_mechanism).
narrative_ontology:affects_constraint(end_of_life_authority__sanctity_reading, physician_conscience_protections).
narrative_ontology:affects_constraint(end_of_life_authority__sanctity_reading, palliative_care_access_mandates).
narrative_ontology:affects_constraint(end_of_life_authority__sanctity_reading, advance_directive_legal_frameworks).

% DUAL FORMULATION NOTE:
% This is one of three constraint stories decomposing the end_of_life_authority kernel. The sanctity_reading claims Mountain-like naturalness but operates as a Tangled Rope with identifiable beneficiaries and victims. The autonomy_reading claims Rope-like coordination but may operate as a Snare for vulnerable populations (per the slippery_slope_mechanism reading). The slippery_slope_mechanism is an empirical claim about constraint dynamics, not a normative reading. All three are linked via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(end_of_life_authority__sanctity_reading, institutional, 0.15).
constraint_indexing:directionality_override(end_of_life_authority__sanctity_reading, organized, 0.25).
constraint_indexing:directionality_override(end_of_life_authority__sanctity_reading, powerless, 0.85).
constraint_indexing:directionality_override(end_of_life_authority__sanctity_reading, moderate, 0.7).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
