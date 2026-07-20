% ============================================================================
% CONSTRAINT STORY: refugee_convention_text__restrictive_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_refugee_convention_text__restrictive_sovereignty_reading, []).

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
 *   constraint_id: refugee_convention_text__restrictive_sovereignty_reading
 *   human_readable: Restrictive Sovereignty Reading of the 1951 Refugee Convention
 *   domain: international_law/migration_governance/human_rights
 *
 * SUMMARY:
 *   The 1951 Refugee Convention is a contested kernel generating multiple
 *   structurally distinct interpretive constraints. This story instantiates
 *   the restrictive sovereignty reading: the Convention is treated as a
 *   minimum floor permitting maximum sovereign discretion. Under this
 *   reading, 'well-founded fear' requires individualized proof of persecution
 *   with state awareness or acquiescence; 'particular social group' is
 *   limited to immutable characteristics; and offshore processing, pushbacks,
 *   and high admissibility barriers are permissible. The reading narrows the
 *   protected class and excludes those fleeing generalized violence,
 *   non-state actor persecution without state nexus, and many gender-based
 *   claims. It functions as a tangled rope: it genuinely coordinates
 *   protection for those who meet narrow criteria, while extracting from
 *   excluded asylum seekers by legitimizing their refoulement and detention
 *   through formal legal interpretation.
 *
 * KEY AGENTS:
 *   - sovereign_states: Primary agenda-setter (institutional/constrained) â sets narrow interpretive criteria and benefits from preserved territorial discretion
 *   - excluded_asylum_seekers: Primary payer (powerless/trapped) â bears cost of restrictive admissibility criteria, offshore exclusion, and refoulement
 *   - recognized_refugees: Beneficiary (moderate/constrained) â receives protection within narrow criteria but remains precariously situated
 *   - unhcr: Observer (institutional/analytical) â supervises but cannot override sovereign interpretation
 *   - human_rights_courts_and_bodies: Observer (institutional/analytical) â promote expansive interpretation but lack enforcement against restrictive states
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(refugee_convention_text__restrictive_sovereignty_reading, 0.78).
domain_priors:suppression_score(refugee_convention_text__restrictive_sovereignty_reading, 0.85).
domain_priors:theater_ratio(refugee_convention_text__restrictive_sovereignty_reading, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(refugee_convention_text__restrictive_sovereignty_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(refugee_convention_text__restrictive_sovereignty_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(refugee_convention_text__restrictive_sovereignty_reading, theater_ratio, 0.68).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(refugee_convention_text__restrictive_sovereignty_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(refugee_convention_text__restrictive_sovereignty_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(refugee_convention_text__restrictive_sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(refugee_convention_text__restrictive_sovereignty_reading, "Restrictive Sovereignty Reading of the 1951 Refugee Convention").
narrative_ontology:topic_domain(refugee_convention_text__restrictive_sovereignty_reading, "international_law/migration_governance/human_rights").

domain_priors:requires_active_enforcement(refugee_convention_text__restrictive_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(refugee_convention_text__restrictive_sovereignty_reading, '47a9a5c4-ec04-4ef7-b9eb-688b6d2f8888').
narrative_ontology:cs_kernel_codification('47a9a5c4-ec04-4ef7-b9eb-688b6d2f8888', fixed_text).
narrative_ontology:cs_authority_grounding('47a9a5c4-ec04-4ef7-b9eb-688b6d2f8888', lineage).
narrative_ontology:cs_interpretation_layer_present('47a9a5c4-ec04-4ef7-b9eb-688b6d2f8888').
narrative_ontology:cs_reading_relation('47a9a5c4-ec04-4ef7-b9eb-688b6d2f8888', refugee_convention_text__expansive_humanitarian_reading, coexists_with).
narrative_ontology:cs_reading_relation('47a9a5c4-ec04-4ef7-b9eb-688b6d2f8888', refugee_convention_text__procedural_integrity_reading, coexists_with).
narrative_ontology:cs_axiom('47a9a5c4-ec04-4ef7-b9eb-688b6d2f8888', foundational, state_consent_over_humanitarian_expansion).
narrative_ontology:cs_axiom_status(state_consent_over_humanitarian_expansion, holdable).
narrative_ontology:cs_axiom_grounding('47a9a5c4-ec04-4ef7-b9eb-688b6d2f8888', state_consent_over_humanitarian_expansion, conventional).
narrative_ontology:cs_axiom('47a9a5c4-ec04-4ef7-b9eb-688b6d2f8888', foundational, immutability_requirement_for_social_group).
narrative_ontology:cs_axiom_status(immutability_requirement_for_social_group, holdable).
narrative_ontology:cs_axiom_grounding('47a9a5c4-ec04-4ef7-b9eb-688b6d2f8888', immutability_requirement_for_social_group, conventional).
narrative_ontology:cs_reference_frame('47a9a5c4-ec04-4ef7-b9eb-688b6d2f8888', restrictive_state_consent_framework).
narrative_ontology:cs_drift_state('47a9a5c4-ec04-4ef7-b9eb-688b6d2f8888', contemporary_protection_expansion_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('47a9a5c4-ec04-4ef7-b9eb-688b6d2f8888', '').
narrative_ontology:cs_kernel_id(refugee_convention_text__restrictive_sovereignty_reading, refugee_convention_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(refugee_convention_text__restrictive_sovereignty_reading, sovereign_states).
narrative_ontology:constraint_beneficiary(refugee_convention_text__restrictive_sovereignty_reading, recognized_refugees).
narrative_ontology:constraint_victim(refugee_convention_text__restrictive_sovereignty_reading, excluded_asylum_seekers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interpret the Convention narrowly to preserve territorial sovereignty and discretion over admission. Set admissibility criteria requiring individualized persecution proof backed by state awareness, limit particular social group to immutable characteristics, and deploy offshore processing and pushbacks to avoid territorial jurisdiction triggers. Benefit from reduced protection obligations and maintained border control.
narrative_ontology:constraint_stakeholder(refugee_convention_text__restrictive_sovereignty_reading, sovereign_states, agenda_setter,
    institutional, generational, constrained, global).

% Flee generalized violence, non-state persecution, gender-based persecution, or severe socioeconomic deprivation but are excluded because the restrictive reading demands individualized state-aware persecution and immutable social-group traits. Face pushbacks, offshore detention, or refoulement because the narrow criteria do not cover their displacement.
narrative_ontology:constraint_stakeholder(refugee_convention_text__restrictive_sovereignty_reading, excluded_asylum_seekers, payer,
    powerless, immediate, trapped, regional).

% Meet the restrictive reading's narrow criteria and receive Convention protection, including non-refoulement and legal status. Their protection depends on the Convention's continued operation, even under restrictive interpretation, but they remain vulnerable to narrowed revocation and exclusionary deterrence.
narrative_ontology:constraint_stakeholder(refugee_convention_text__restrictive_sovereignty_reading, recognized_refugees, beneficiary,
    moderate, biographical, constrained, national).

% Mandated to supervise Convention application and promote refugee protection. Frequently contests restrictive state interpretations through guidelines and interventions, but lacks enforcement authority against sovereign determinations and is dependent on state consent for field access.
narrative_ontology:constraint_stakeholder(refugee_convention_text__restrictive_sovereignty_reading, unhcr, observer,
    institutional, generational, analytical, global).

% Issue judgments and opinions interpreting Convention provisions expansively, including gender-based persecution and non-state actor claims. Their authority is resisted or disregarded by states adhering to the restrictive reading, especially where territorial jurisdiction is contested.
narrative_ontology:constraint_stakeholder(refugee_convention_text__restrictive_sovereignty_reading, human_rights_courts_and_bodies, observer,
    institutional, generational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(refugee_convention_text__restrictive_sovereignty_reading, sovereign_states).
narrative_ontology:fixing_cost_class(refugee_convention_text__restrictive_sovereignty_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared international framework for identifying and protecting refugees fleeing persecution, enabling states to coordinate responses to displacement without unilateral absorption of all protection costs.
% TRANSFER_FUNCTION: Moves the obligation to provide territorial asylum and legal status onto a narrow subset of claimants who can prove individualized, state-aware persecution based on immutable characteristics; moves the cost of exclusion (pushbacks, detention, refoulement) onto asylum seekers fleeing generalized violence and non-state persecution.
% ABSENT_VOICES: Asylum seekers fleeing climate displacement, generalized gang violence, and severe socioeconomic deprivation are structurally excluded because the restrictive reading frames their claims as outside the Convention. Their voices appear only in intercepted boats, detention center records, and pushback narratives, not in the interpretive conversation.
% DISAPPEARANCE_RATIONALE: If the restrictive reading vanished and states applied expansive or procedural readings instead, protection obligations would broaden significantly, offshore processing regimes would likely collapse, and border enforcement paradigms would shift from exclusion to individualized fair assessment. The global migration governance landscape would rearrange around broader protection duties and reduced sovereign discretion.
% FOUNDING_PROBLEM: Post-WWII displacement crisis requiring a cooperative framework to identify and protect refugees while preserving state sovereignty over immigration and border control.
% FOUNDING_PROBLEM_CORROBORATION: States adhering to the restrictive reading attest the problem is still live, citing sovereignty and unregulated migration threats. Human rights organizations and UNHCR attest the founding displacement problem has evolved to include generalized violence and non-state persecution, and that the restrictive arrangement persists to minimize state obligations rather than solve contemporary displacement. Historical records of the 1951 Conference corroborate mixed intent: some delegations emphasized humanitarian protection, while state archives and negotiating records corroborate sovereignty-preservation intent among major receiving states.
narrative_ontology:disappearance_verdict(refugee_convention_text__restrictive_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(refugee_convention_text__restrictive_sovereignty_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(refugee_convention_text__restrictive_sovereignty_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(refugee_convention_text__restrictive_sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(refugee_convention_text__restrictive_sovereignty_reading, 0.78, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(refugee_convention_text__restrictive_sovereignty_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(refugee_convention_text__restrictive_sovereignty_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(refugee_convention_text__restrictive_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.78 at interval end) because the restrictive reading systematically excludes large categories of vulnerable migrants from protection by narrowing 'persecution' and 'social group' to state-aware individualized harms and immutable traits. Suppression is very high (0.85) because the constraint's persistence depends on active border enforcement, pushbacks, detention, and suppression of expansive legal alternatives. Theater_ratio is substantial (0.68) because states perform compliance with international refugee law while operating offshore and pushback regimes that hollow out protection in practice. Accessibility_collapse is moderate-high (0.70) because once the restrictive reading is adopted, alternatives (expansive interpretation, territorial jurisdiction, gender-based protection) appear legally closed to adopting states. Resistance is moderate (0.55) because excluded migrants and human rights bodies actively contest the reading, but sovereign states collectively maintain it through interpretive autonomy and non-refoulement evasion.
 *
 * PERSPECTIVAL GAP:
 *   The sovereign state seat experiences the constraint as a necessary defense of territorial sovereignty and a cooperative framework to manage displacement without unilateral burden. The excluded asylum seeker seat experiences the same legal text as an extraction mechanism that legitimizes their return to danger. The recognized refugee sits between: they benefit from the Convention's protection but may also experience the restrictive reading as a source of precarity (narrow criteria, temporary protection, threat of status revocation). The engine computes this divergence from the structural data rather than adjudicating it.
 *
 * DIRECTIONALITY LOGIC:
 *   Sovereign_states are structural beneficiaries (agenda_setters who control interpretation and enforcement â d near 0.0). Excluded_asylum_seekers are structural victims (pay through refoulement, detention, and exclusion â d near 1.0). Recognized_refugees are partial beneficiaries (protected but within a narrowed framework â d around 0.3). UNHCR and human rights courts are analytical observers with d near 0.5 (symmetrically invested in the regime's survival but not its restrictive operation).
 *
 * MANDATROPHY ANALYSIS:
 *   The restrictive reading resists mandatrophy mislabeling because it retains a genuine coordination function: it protects refugees who meet narrow criteria and provides a stable interstate framework. Without that coordination function, it would read as a pure snare (legal text as cover for exclusion). The presence of recognized_refugees as beneficiaries and the Convention's partial protection function prevent that reclassification. However, the high theater_ratio and rising extractiveness over time indicate that the coordination function is increasingly performed for show while the extraction function dominates.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    restrictive_reading_kernel_contest,
    'Is the restrictive sovereignty reading a faithful interpretation of the 1951 Convention text, or does it construct extraction by layering sovereign discretion onto a humanitarian kernel?',
    'Comparative legal analysis of the travaux prÃ©paratoires, subsequent practice under VCLT Articles 31 and 32, and divergence tracking across state jurisdictions and regional human rights bodies.',
    'If the reading is constructed extraction, the epsilon is reading-dependent and the kernel decomposes into multiple constraints; if faithful, the extraction is inherent to the coordination cost of state consent and this reading is the correct structural model.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(restrictive_reading_kernel_contest, conceptual, 'Whether the restrictive reading derives from the kernel or constructs extraction upon it').

omega_variable(
    social_group_immutability_construction,
    'Does the particular social group limitation to immutable characteristics reflect the Convention''s textual meaning, or is it a state-constructed filter to exclude gender-based, LGBTQ+, and clan-based claims?',
    'Systematic review of domestic and international jurisprudence on social group definitional boundaries; analysis of state practice pre- and post-Acarta Guidelines and UNHCR guidance.',
    'Resolving toward textual inclusion would reclassify the victim set and reduce extractiveness; resolving toward constructed filter would confirm the extraction mechanism and support re-reading as snare-leaning.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(social_group_immutability_construction, empirical, 'Whether social group immutability is textual or constructed exclusion').

omega_variable(
    non_state_actor_state_nexus,
    'Does the restrictive reading''s state-awareness requirement for non-state persecution correctly interpret the Convention, or does it construct an exclusion absent from the treaty''s object and purpose?',
    'Historical analysis of drafter intent regarding non-state persecution, combined with contemporary empirical study of displacement drivers.',
    'If the state-nexus requirement is a construction, a significant portion of the victim set has been artificially generated by interpretive extraction rather than textual fidelity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(non_state_actor_state_nexus, empirical, 'Whether non-state persecution exclusion is textual or constructed').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(refugee_convention_text__restrictive_sovereignty_reading, 0, 70).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rcr_restrictive_tr_t0, refugee_convention_text__restrictive_sovereignty_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(rcr_restrictive_tr_t15, refugee_convention_text__restrictive_sovereignty_reading, theater_ratio, 15, 0.25).
narrative_ontology:measurement(rcr_restrictive_tr_t30, refugee_convention_text__restrictive_sovereignty_reading, theater_ratio, 30, 0.35).
narrative_ontology:measurement(rcr_restrictive_tr_t45, refugee_convention_text__restrictive_sovereignty_reading, theater_ratio, 45, 0.5).
narrative_ontology:measurement(rcr_restrictive_tr_t60, refugee_convention_text__restrictive_sovereignty_reading, theater_ratio, 60, 0.6).
narrative_ontology:measurement(rcr_restrictive_tr_t70, refugee_convention_text__restrictive_sovereignty_reading, theater_ratio, 70, 0.68).

% Extraction over time
narrative_ontology:measurement(rcr_restrictive_be_t0, refugee_convention_text__restrictive_sovereignty_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(rcr_restrictive_be_t15, refugee_convention_text__restrictive_sovereignty_reading, base_extractiveness, 15, 0.45).
narrative_ontology:measurement(rcr_restrictive_be_t30, refugee_convention_text__restrictive_sovereignty_reading, base_extractiveness, 30, 0.55).
narrative_ontology:measurement(rcr_restrictive_be_t45, refugee_convention_text__restrictive_sovereignty_reading, base_extractiveness, 45, 0.65).
narrative_ontology:measurement(rcr_restrictive_be_t60, refugee_convention_text__restrictive_sovereignty_reading, base_extractiveness, 60, 0.72).
narrative_ontology:measurement(rcr_restrictive_be_t70, refugee_convention_text__restrictive_sovereignty_reading, base_extractiveness, 70, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(rcr_restrictive_su_t0, refugee_convention_text__restrictive_sovereignty_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(rcr_restrictive_su_t15, refugee_convention_text__restrictive_sovereignty_reading, suppression_requirement, 15, 0.38).
narrative_ontology:measurement(rcr_restrictive_su_t30, refugee_convention_text__restrictive_sovereignty_reading, suppression_requirement, 30, 0.5).
narrative_ontology:measurement(rcr_restrictive_su_t45, refugee_convention_text__restrictive_sovereignty_reading, suppression_requirement, 45, 0.65).
narrative_ontology:measurement(rcr_restrictive_su_t60, refugee_convention_text__restrictive_sovereignty_reading, suppression_requirement, 60, 0.78).
narrative_ontology:measurement(rcr_restrictive_su_t70, refugee_convention_text__restrictive_sovereignty_reading, suppression_requirement, 70, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(refugee_convention_text__restrictive_sovereignty_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(refugee_convention_text__restrictive_sovereignty_reading, refugee_convention_text__expansive_humanitarian_reading).
narrative_ontology:affects_constraint(refugee_convention_text__restrictive_sovereignty_reading, refugee_convention_text__procedural_integrity_reading).

% DUAL FORMULATION NOTE:
% This constraint is the restrictive_sovereignty_reading of the refugee_convention_text kernel. It is structurally distinct from the expansive_humanitarian_reading (which broadens protection to generalized violence and non-state persecution) and the procedural_integrity_reading (which prioritizes fair process over substantive outcome). The epsilon values differ because the victim sets, beneficiary structures, and extraction mechanisms differ across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
