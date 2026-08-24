% ============================================================================
% CONSTRAINT STORY: dignified_death__sanctity_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
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
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   human_readable: Sanctity-of-Life Prohibition on Assisted Dying
 *   domain: bioethics/medical_law/political_philosophy
 *
 * SUMMARY:
 *   The sanctity_primary reading of the dignified_death kernel asserts that
 *   human life possesses intrinsic value grounded in transcendent moral law,
 *   making intentional life-termination categorically impermissible
 *   regardless of consent. This reading instantiates a snare constraint: the
 *   protection norm against coercive killing becomes a coercive prolongation
 *   of suffering for vulnerable populations (elderly, disabled, poor) who
 *   lack exit options. The constraint extracts continued life/suffering from
 *   these groups to sustain a moral order that benefits religious
 *   institutions and advocacy organizations. Legal enforcement actively
 *   suppresses alternatives (travel, VSED, underground assistance) and the
 *   theater ratio rises as palliative care rhetoric increasingly masks the
 *   constraint's extractive core.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dignified_death__sanctity_primary, 0.58).
domain_priors:suppression_score(dignified_death__sanctity_primary, 0.75).
domain_priors:theater_ratio(dignified_death__sanctity_primary, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dignified_death__sanctity_primary, extractiveness, 0.58).
narrative_ontology:constraint_metric(dignified_death__sanctity_primary, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(dignified_death__sanctity_primary, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dignified_death__sanctity_primary, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(dignified_death__sanctity_primary, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dignified_death__sanctity_primary, snare).
narrative_ontology:human_readable(dignified_death__sanctity_primary, "Sanctity-of-Life Prohibition on Assisted Dying").
narrative_ontology:topic_domain(dignified_death__sanctity_primary, "bioethics/medical_law/political_philosophy").

domain_priors:requires_active_enforcement(dignified_death__sanctity_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dignified_death__sanctity_primary, '031af7a2-3955-4cf9-b720-2598d0d26ebe').
narrative_ontology:cs_kernel_codification('031af7a2-3955-4cf9-b720-2598d0d26ebe', formalized).
narrative_ontology:cs_authority_grounding('031af7a2-3955-4cf9-b720-2598d0d26ebe', lineage).
narrative_ontology:cs_interpretation_layer_present('031af7a2-3955-4cf9-b720-2598d0d26ebe').
narrative_ontology:cs_reading_relation('031af7a2-3955-4cf9-b720-2598d0d26ebe', dignified_death__autonomy_primary, coexists_with).
narrative_ontology:cs_reading_relation('031af7a2-3955-4cf9-b720-2598d0d26ebe', dignified_death__relational_autonomy, coexists_with).
narrative_ontology:cs_axiom('031af7a2-3955-4cf9-b720-2598d0d26ebe', foundational, life_intrinsic_value_prohibits_intentional_termination).
narrative_ontology:cs_axiom_status(life_intrinsic_value_prohibits_intentional_termination, holdable).
narrative_ontology:cs_axiom_grounding('031af7a2-3955-4cf9-b720-2598d0d26ebe', life_intrinsic_value_prohibits_intentional_termination, deontological).
narrative_ontology:cs_reference_frame('031af7a2-3955-4cf9-b720-2598d0d26ebe', classical_sanctity_framework).
narrative_ontology:cs_drift_state('031af7a2-3955-4cf9-b720-2598d0d26ebe', contemporary_rights_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('031af7a2-3955-4cf9-b720-2598d0d26ebe', '').
narrative_ontology:cs_kernel_id(dignified_death__sanctity_primary, dignified_death).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dignified_death__sanctity_primary, religious_institutions).
narrative_ontology:constraint_beneficiary(dignified_death__sanctity_primary, pro_life_advocacy_organizations).
narrative_ontology:constraint_victim(dignified_death__sanctity_primary, elderly_patients).
narrative_ontology:constraint_victim(dignified_death__sanctity_primary, disabled_patients).
narrative_ontology:constraint_victim(dignified_death__sanctity_primary, low_income_patients).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(dignified_death__sanctity_primary, palliative_care_establishment).
narrative_ontology:constraint_vindicates(dignified_death__sanctity_primary, sanctity_of_life_doctrine).
narrative_ontology:constraint_vindicates(dignified_death__sanctity_primary, transcendent_moral_law_prohibits_killing).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain doctrinal authority over life and death through theological teaching, political lobbying, and institutional influence on healthcare systems. They define the moral framework that prohibits intentional life-termination and mobilize congregations and political allies to sustain legal prohibitions. Their authority derives from claimed continuity with transcendent moral law.
narrative_ontology:constraint_stakeholder(dignified_death__sanctity_primary, religious_institutions, agenda_setter,
    institutional, generational, analytical, global).

% Build organizational capacity, funding streams, and political capital around defending the prohibition. They litigate, lobby, and run public campaigns framing assisted dying as a threat to vulnerable populations. The constraint's persistence validates their mission and sustains their donor base and policy influence.
narrative_ontology:constraint_stakeholder(dignified_death__sanctity_primary, pro_life_advocacy_organizations, beneficiary,
    organized, biographical, mobile, national).

% Face prolonged dying with diminished capacity, often dependent on others for care. The prohibition denies them legal access to assisted dying even when they experience intolerable suffering. Exit options include traveling to permissive jurisdictions (costly, requires capacity), voluntary stopping of eating and drinking (physically arduous), or enduring to natural death. Family pressure and internalized burden feelings compound coercion.
narrative_ontology:constraint_stakeholder(dignified_death__sanctity_primary, elderly_patients, payer,
    moderate, immediate, constrained, national).

% Live with chronic conditions that the prohibition frames as 'life worth living' regardless of the patient's own assessment. They face structural pressure to accept continued existence as a moral witness. Disability rights advocates opposing legalization argue from within this seat that 'choice' under ableist conditions is illusory; the constraint uses their lives as evidence for its own necessity while denying them exit.
narrative_ontology:constraint_stakeholder(dignified_death__sanctity_primary, disabled_patients, payer,
    powerless, biographical, trapped, national).

% Lack resources to travel for assisted dying or to secure high-quality palliative care. The prohibition binds them most tightly: they cannot buy exit, and the safety-net healthcare system they rely on is shaped by the prohibition's institutional logic. They bear the extraction of prolonged suffering without the mitigation wealth provides.
narrative_ontology:constraint_stakeholder(dignified_death__sanctity_primary, low_income_patients, payer,
    powerless, immediate, trapped, national).

% Professional jurisdiction over end-of-life care is protected by the prohibition; legalization would restructure their role and introduce competing services. Many clinicians genuinely believe the prohibition protects patients, but the institutional arrangement also secures their clinical monopoly and moral authority in the dying process.
narrative_ontology:constraint_stakeholder(dignified_death__sanctity_primary, palliative_care_establishment, beneficiary,
    organized, biographical, mobile, national).

% Argue that dignity requires self-determination over death timing. In prohibition jurisdictions they are structurally excluded from policy-making; their clients (patients seeking assisted dying) are criminalized or forced underground. They testify in legislative hearings and litigate constitutional challenges but the constraint's enforcement machinery treats their position as illegitimate.
narrative_ontology:constraint_stakeholder(dignified_death__sanctity_primary, autonomy_advocates, excluded,
    organized, biographical, analytical, national).

% Analyze the constraint's empirical effects, conceptual coherence, and distributive justice. They document coercion mechanisms, track jurisdiction-level outcomes, and map the argumentative structure of the debate. Their work feeds courts and legislatures but they hold no enforcement power.
narrative_ontology:constraint_stakeholder(dignified_death__sanctity_primary, bioethics_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Protects vulnerable populations from coercion into premature death; maintains social trust in medical profession as healers not killers; upholds shared moral framework against instrumentalization of life.
% TRANSFER_FUNCTION: Moves the burden of continued suffering from the collective (which would bear moral responsibility for permitting killing) onto vulnerable individuals who must endure prolonged dying; moves moral authority from individual to transcendent law/community.
% ABSENT_VOICES: Vulnerable patients who would choose assisted dying but cannot speak (demented, unconscious); patients in jurisdictions without access who suffer silently; future patients who will face the same constraint.
% DISAPPEARANCE_RATIONALE: Legalization would create regulated pathways for assisted dying, shifting medical practice, altering end-of-life conversations, redistributing moral authority from transcendent law to individual autonomy, and changing resource allocation in palliative care.
% FOUNDING_PROBLEM: The prohibition was built to solve the problem of state-sanctioned killing (eugenics, involuntary euthanasia, medical abuse of vulnerable populations) and to protect the medical profession's role as healer.
% FOUNDING_PROBLEM_CORROBORATION: Historical records of Nazi eugenics programs and pre-legalization abuse documented by historians outside the benefiting parties; disability rights organizations (not beneficiaries of prohibition) attest that coercion risks remain live; medical historians corroborate the professional identity concern.
narrative_ontology:disappearance_verdict(dignified_death__sanctity_primary, world_rearranges).
narrative_ontology:founding_problem_status(dignified_death__sanctity_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dignified_death__sanctity_primary, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(dignified_death__sanctity_primary, 'none', 1).
narrative_ontology:epsilon_provenance(dignified_death__sanctity_primary, 0.58, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Base extractiveness (0.58) reflects the substantial transfer of suffering-burden onto vulnerable patients who cannot access assisted dying; the constraint's coordination function (preventing coercion) is real but the extraction exceeds the coordination benefit for the named victims. Suppression (0.75) is high because the prohibition is maintained by criminal law, medical licensing, and institutional gatekeeping — not by participant preference. Theater ratio (0.30) captures the growing gap between the proclaimed protective purpose and the lived reality of prolonged suffering. Accessibility collapse (0.70) is high but not total: some patients travel to permissive jurisdictions or use VSED, but these exits are costly and capacity-dependent. Resistance (0.60) reflects sustained litigation, legislative challenges, and shifting public opinion.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat (religious institutions), the constraint is a genuine coordination mechanism protecting the vulnerable from a genuine historical threat (eugenics, involuntary euthanasia). From the payer seats (vulnerable patients), the same structure operates as enforced extraction — the protection they are told to be grateful for is the very mechanism that denies them relief. The engine computes this divergence from the declared roles and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   Religious institutions and pro-life organizations are structural beneficiaries (agenda_setter and beneficiary roles) — they collect moral authority, political capital, and institutional preservation from the constraint (d near 0.0). Elderly, disabled, and low-income patients are structural targets (payer role) — they bear the extraction of forced continued suffering with trapped or constrained exit (d near 1.0). Palliative care establishment sits near beneficiary (professional jurisdiction protected). Autonomy advocates are excluded (their voice would challenge the constraint's legitimacy). Bioethics scholars are analytical observers.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing state-sanctioned killing of the vulnerable) is historically real but its status is contested: disability rights advocates argue new coercion forms would emerge under legalization; autonomy advocates argue the original problem is solved by modern safeguards. The constraint persists not because the founding problem is live, but because the benefiting institutions (religious, pro-life orgs, palliative care establishment) have built identities and revenue streams on the prohibition. This is mandatrophy: the mandate (protect vulnerable) has atrophied into a snare that extracts from the very populations it claims to protect.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    committer_structure_sanctity_primary,
    'How does the kernel/reading structure affect the constraint''s classification — specifically, does the sanctity_primary reading''s claim to transcendent authority mask a constructed constraint that extracts from vulnerable populations?',
    'Compare the constraint''s empirical operation (extraction from vulnerable, active suppression of alternatives) across jurisdictions with and without legalization. If the protection rationale holds only where enforcement is total and collapses where alternatives exist, the transcendent claim is a cover for extraction.',
    'If the transcendent moral law claim is a constructed cover, the constraint is a snare with high effective extraction for vulnerable populations. If the claim is structurally genuine (the constraint would persist even without beneficiaries), it approaches a mountain — but the declared beneficiaries and high extraction make this unlikely.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(committer_structure_sanctity_primary, conceptual, 'Whether the sanctity_primary reading''s kernel commitment is a genuine natural-law constraint or a constructed snare using transcendent language.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (legal prohibition, medical gatekeeping) or internalized (patients believe they ought not choose death, have fused identity with suffering)?',
    'Post-legalization suppression trajectory in jurisdictions that permit assisted dying: if patients still refrain from accessing it due to internalized moral prohibition, reclassify part of the suppression as internalized.',
    'If internalized suppression is substantial, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the suppression with them even after legal exit opens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for vulnerable populations under the prohibition.').

omega_variable(
    coercion_vs_protection_boundary,
    'At what point does the protection norm against coercion become coercion itself — forcing continued suffering on those it claims to protect?',
    'Longitudinal study of patient requests for assisted dying in prohibition vs. legalization jurisdictions: if request rates are similar but fulfillment differs, the constraint is extracting choice from a stable preference.',
    'If preference for assisted dying exists independent of legal regime, the prohibition''s extraction is pure — it blocks a pre-existing choice rather than preventing a coerced one.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coercion_vs_protection_boundary, empirical, 'Whether the constraint prevents coercion or constitutes it.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dignified_death__sanctity_primary, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dign_tr_t0, dignified_death__sanctity_primary, theater_ratio, 0, 0.15).
narrative_ontology:measurement(dign_tr_t10, dignified_death__sanctity_primary, theater_ratio, 10, 0.18).
narrative_ontology:measurement(dign_tr_t20, dignified_death__sanctity_primary, theater_ratio, 20, 0.22).
narrative_ontology:measurement(dign_tr_t30, dignified_death__sanctity_primary, theater_ratio, 30, 0.26).
narrative_ontology:measurement(dign_tr_t40, dignified_death__sanctity_primary, theater_ratio, 40, 0.28).
narrative_ontology:measurement(dign_tr_t50, dignified_death__sanctity_primary, theater_ratio, 50, 0.3).

% Extraction over time
narrative_ontology:measurement(dign_be_t0, dignified_death__sanctity_primary, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(dign_be_t10, dignified_death__sanctity_primary, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(dign_be_t20, dignified_death__sanctity_primary, base_extractiveness, 20, 0.5).
narrative_ontology:measurement(dign_be_t30, dignified_death__sanctity_primary, base_extractiveness, 30, 0.55).
narrative_ontology:measurement(dign_be_t40, dignified_death__sanctity_primary, base_extractiveness, 40, 0.57).
narrative_ontology:measurement(dign_be_t50, dignified_death__sanctity_primary, base_extractiveness, 50, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(dign_su_t0, dignified_death__sanctity_primary, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(dign_su_t10, dignified_death__sanctity_primary, suppression_requirement, 10, 0.65).
narrative_ontology:measurement(dign_su_t20, dignified_death__sanctity_primary, suppression_requirement, 20, 0.7).
narrative_ontology:measurement(dign_su_t30, dignified_death__sanctity_primary, suppression_requirement, 30, 0.72).
narrative_ontology:measurement(dign_su_t40, dignified_death__sanctity_primary, suppression_requirement, 40, 0.74).
narrative_ontology:measurement(dign_su_t50, dignified_death__sanctity_primary, suppression_requirement, 50, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dignified_death__sanctity_primary, identity_coordination).
narrative_ontology:boltzmann_floor_override(dignified_death__sanctity_primary, 0.08).
narrative_ontology:affects_constraint(dignified_death__sanctity_primary, dignified_death__autonomy_primary).
narrative_ontology:affects_constraint(dignified_death__sanctity_primary, dignified_death__relational_autonomy).

% DUAL FORMULATION NOTE:
% The dignified_death kernel decomposes into three readings with distinct ε values and victim/beneficiary structures. Sanctity_primary (this story) has ε≈0.58, victims=vulnerable populations, beneficiaries=religious/pro-life institutions. Autonomy_primary has ε≈0.15 (low extraction, coordination of individual choice), beneficiaries=autonomy-seeking patients. Relational_autonomy has ε≈0.35 (moderate extraction via procedural burdens), victims=patients failed by safeguards. The three stories form a constraint family linked by affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(dignified_death__sanctity_primary, institutional, 0.1).
constraint_indexing:directionality_override(dignified_death__sanctity_primary, powerless, 0.9).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
