% ============================================================================
% CONSTRAINT STORY: validation_judgment_separation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-09
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_validation_judgment_separation, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: validation_judgment_separation
 *   human_readable: Validation-Judgment Separation in Knowledge Production
 *   domain: epistemology/institutional_science
 *
 * SUMMARY:
 *   Large language models have collapsed the friction of generating coherent
 *   cross-domain syntheses in biomedicine and other complex domains. A
 *   synthesizer with domain literacy can now produce mechanistically
 *   plausible frameworks connecting disparate evidence in hours rather than
 *   months. However, these tools cannot reliably distinguish their own valid
 *   insights from confabulations—they generate coherent narratives whether or
 *   not the underlying mechanisms are true. Domain expertise remains
 *   structurally necessary for validation judgment: assessing whether a
 *   mechanistic claim is not just coherent but correct, whether component
 *   evidence actually supports the synthesis, whether alternative
 *   explanations are ruled out. This separation between synthesis capacity
 *   (now LLM-augmented) and validation judgment (still requiring human
 *   expertise) is presented as a mountain—an irreducible epistemic
 *   constraint. But institutional gatekeepers who control validation pathways
 *   benefit from maintaining high barriers to legitimacy, and the
 *   constraint's operation increasingly serves to preserve institutional
 *   authority over knowledge claims rather than purely to protect against
 *   false positives.
 *
 * KEY AGENTS:
 *   - institutional_gatekeepers: Agenda-setters (institutional/arbitrage) — control peer review and credentialing; benefit from high barriers
 *   - credentialed_domain_experts: Beneficiaries (powerful/mobile) — possess irreplaceable judgment capacity; benefit from institutional structures
 *   - llm_assisted_synthesizers: Payers (moderate/constrained) — generate plausible frameworks but lack validation pathways
 *   - individual_health_decision_makers: Payers (powerless/trapped) — bear costs of both error types under epistemic uncertainty
 *   - research_funding_bodies: Beneficiaries + Agenda-setters (institutional/mobile) — allocate resources based on validation signals
 *   - epistemic_methodologists: Observers (analytical/analytical) — study separation of synthesis and validation functions
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(validation_judgment_separation, 0.12).
domain_priors:suppression_score(validation_judgment_separation, 0.08).
domain_priors:theater_ratio(validation_judgment_separation, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(validation_judgment_separation, extractiveness, 0.12).
narrative_ontology:constraint_metric(validation_judgment_separation, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(validation_judgment_separation, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(validation_judgment_separation, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(validation_judgment_separation, resistance, 0.06).

% --- Constraint claim ---
narrative_ontology:constraint_claim(validation_judgment_separation, mountain).
narrative_ontology:human_readable(validation_judgment_separation, "Validation-Judgment Separation in Knowledge Production").
narrative_ontology:topic_domain(validation_judgment_separation, "epistemology/institutional_science").

domain_priors:emerges_naturally(validation_judgment_separation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(validation_judgment_separation, '2af2b870-f643-453d-abaf-a49bc4054160').
narrative_ontology:cs_kernel_codification('2af2b870-f643-453d-abaf-a49bc4054160', distributed).
narrative_ontology:cs_authority_grounding('2af2b870-f643-453d-abaf-a49bc4054160', distributed).
narrative_ontology:cs_reading_relation('2af2b870-f643-453d-abaf-a49bc4054160', validation_judgment_separation__institutional_validation_reading, coexists_with).
narrative_ontology:cs_reading_relation('2af2b870-f643-453d-abaf-a49bc4054160', validation_judgment_separation__pragmatic_action_reading, coexists_with).
narrative_ontology:cs_axiom('2af2b870-f643-453d-abaf-a49bc4054160', foundational, synthesis_validation_separability).
narrative_ontology:cs_axiom_status(synthesis_validation_separability, holdable).
narrative_ontology:cs_axiom_grounding('2af2b870-f643-453d-abaf-a49bc4054160', synthesis_validation_separability, empirically_contingent).
narrative_ontology:cs_axiom('2af2b870-f643-453d-abaf-a49bc4054160', foundational, expert_judgment_irreplaceability).
narrative_ontology:cs_axiom_status(expert_judgment_irreplaceability, holdable).
narrative_ontology:cs_axiom_grounding('2af2b870-f643-453d-abaf-a49bc4054160', expert_judgment_irreplaceability, empirically_contingent).
narrative_ontology:cs_axiom('2af2b870-f643-453d-abaf-a49bc4054160', secondary, institutional_gatekeeping_necessity).
narrative_ontology:cs_axiom_status(institutional_gatekeeping_necessity, holdable).
narrative_ontology:cs_axiom_grounding('2af2b870-f643-453d-abaf-a49bc4054160', institutional_gatekeeping_necessity, conventional).
narrative_ontology:cs_reference_frame('2af2b870-f643-453d-abaf-a49bc4054160', institutional_validation_primacy).
narrative_ontology:cs_drift_state('2af2b870-f643-453d-abaf-a49bc4054160', llm_synthesis_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('2af2b870-f643-453d-abaf-a49bc4054160', '').

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(validation_judgment_separation, institutional_gatekeepers).
narrative_ontology:constraint_beneficiary(validation_judgment_separation, credentialed_domain_experts).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(validation_judgment_separation, research_funding_bodies).
narrative_ontology:constraint_victim(validation_judgment_separation, llm_assisted_synthesizers).
narrative_ontology:constraint_victim(validation_judgment_separation, individual_health_decision_makers).
narrative_ontology:constraint_vindicates(validation_judgment_separation, peer_review_necessity).
narrative_ontology:constraint_vindicates(validation_judgment_separation, methodological_rigor_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Control access to legitimacy through peer review, journal publication, and credentialing systems. Set standards for what counts as validated knowledge. Benefit from maintaining high barriers to entry that preserve institutional authority over knowledge claims. Can route around constraints by publishing in alternative venues or founding new journals when existing structures become too rigid.
narrative_ontology:constraint_stakeholder(validation_judgment_separation, institutional_gatekeepers, agenda_setter,
    institutional, generational, arbitrage, global).

% Possess deep domain expertise enabling accurate judgment of mechanistic plausibility and methodological validity. Benefit from institutional structures that make their expertise irreplaceable for validation. Can move between institutions and consulting roles. Their judgment capacity is the genuine coordination function; their institutional position is the extraction layer.
narrative_ontology:constraint_stakeholder(validation_judgment_separation, credentialed_domain_experts, beneficiary,
    powerful, biographical, mobile, national).

% Can generate coherent cross-domain syntheses rapidly using LLM tools but lack institutional validation pathways. Pay the cost of having mechanistically plausible frameworks dismissed without expert evaluation. Constrained by inability to distinguish their own valid insights from plausible confabulations without expert feedback. Cannot exit the need for validation judgment even as synthesis friction collapses.
narrative_ontology:constraint_stakeholder(validation_judgment_separation, llm_assisted_synthesizers, payer,
    moderate, biographical, constrained, global).

% Face health decisions under uncertainty where institutional validation is incomplete or delayed. Bear the cost of both Type I errors (acting on false claims) and Type II errors (failing to act on true but unvalidated claims). Trapped between institutional caution and synthesis proliferation with no reliable filter. Cannot exit the epistemic dependency.
narrative_ontology:constraint_stakeholder(validation_judgment_separation, individual_health_decision_makers, payer,
    powerless, immediate, trapped, local).

% Allocate resources based on institutional validation signals. Benefit from clear gatekeeping that simplifies funding decisions. Also set research priorities that determine which synthesis hypotheses get tested. Can shift funding strategies across institutions and research paradigms.
narrative_ontology:constraint_stakeholder(validation_judgment_separation, research_funding_bodies, beneficiary,
    institutional, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(validation_judgment_separation, research_funding_bodies, agenda_setter).

% Study the structural properties of knowledge validation systems. Observe that synthesis capacity and validation judgment are separable functions with different computational and cognitive requirements. See how LLM tools collapse one barrier while leaving the other intact, and how institutional structures conflate the two functions.
narrative_ontology:constraint_stakeholder(validation_judgment_separation, epistemic_methodologists, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Distinguishes valid mechanistic frameworks from coherent but false narratives in domains where direct experimental proof is incomplete. Protects against systematic propagation of plausible confabulations that could guide harmful action.
% TRANSFER_FUNCTION: Moves epistemic authority from synthesis producers to validation gatekeepers. Transfers time and attention from synthesizers seeking validation to institutional review processes. Concentrates legitimacy-granting power in credentialed expert networks.
% ABSENT_VOICES: Independent researchers without institutional affiliations, patient communities with experiential knowledge, cross-domain synthesizers outside traditional disciplines. They would argue for validation pathways that assess mechanistic coherence and component evidence quality rather than institutional pedigree, but are excluded from standard peer review.
% DISAPPEARANCE_RATIONALE: If the validation-judgment separation vanished—if synthesis tools could reliably self-validate—the entire institutional apparatus of peer review, credentialing, and journal hierarchies would lose its coordination function. Knowledge production would reorganize around mechanistic coherence testing and component evidence aggregation rather than institutional gatekeeping. The genuine need for expert judgment would remain, but its institutional capture would dissolve.
% FOUNDING_PROBLEM: Pre-institutional science lacked systematic protection against individual bias, motivated reasoning, and the propagation of coherent but empirically false theories. No reliable mechanism existed to distinguish valid cross-domain synthesis from confabulation.
% FOUNDING_PROBLEM_CORROBORATION: Epistemic methodologists and historians of science document that the core problem—distinguishing valid from invalid synthesis—remains unsolved by LLM tools alone. Empirical studies from computer science researchers outside the benefiting institutions show LLMs generate plausible but false mechanistic claims at rates requiring expert filtering. The founding problem persists even as synthesis friction collapses.
narrative_ontology:disappearance_verdict(validation_judgment_separation, world_rearranges).
narrative_ontology:founding_problem_status(validation_judgment_separation, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(validation_judgment_separation, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-18',
    'unspecified', 'agent/example_platform_commission.json',
    'claude-sonnet-4-5-20250929', 'unspecified').
narrative_ontology:story_seed(validation_judgment_separation, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(validation_judgment_separation_tests).

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(validation_judgment_separation, ExtMetricName, E),
    domain_priors:suppression_score(validation_judgment_separation, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(validation_judgment_separation),
    narrative_ontology:constraint_metric(validation_judgment_separation, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(validation_judgment_separation, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(validation_judgment_separation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low but rising (0.08 → 0.12) because the genuine coordination function (expert validation judgment) is increasingly layered with institutional rent-seeking (credentialing barriers, journal hierarchies, peer review delays that exceed what validation requires). Suppression is very low (0.08) because alternatives to institutional validation exist (preprint servers, direct synthesis publication, patient communities) even if they lack legitimacy. Theater ratio is low (0.15) because most validation work is genuine expert judgment, though a growing share is performative credentialism. Accessibility collapse is very high (0.88) because the epistemic constraint—that synthesis tools cannot self-validate—is a structural feature of current AI capabilities, nearly independent of institutional arrangements. Resistance is very low (0.06) because the constraint is largely accepted as real even by those who critique institutional gatekeeping. The measurements show modest upward drift in extraction and theater as institutional structures increasingly capture what was originally a pure coordination function.
 *
 * PERSPECTIVAL GAP:
 *   From the institutional gatekeeper seat, the constraint operates as genuine protection against false positives—peer review and credentialing are necessary filters for synthesis quality. From the synthesizer seat, the same structure operates as extraction—valid frameworks are blocked by institutional barriers that exceed what validation requires. From the powerless decision-maker seat, both the mountain (genuine validation need) and the snare (institutional gatekeeping) are simultaneously true and indistinguishable. The engine computes these divergent classifications from the structural positions; the claimed type (mountain) represents the institutional framing, while the metrics capture the extractive drift.
 *
 * DIRECTIONALITY LOGIC:
 *   Institutional gatekeepers are structural beneficiaries (d ≈ 0.15): they coordinate validation but also extract rents from controlling legitimacy pathways. Credentialed experts are beneficiaries (d ≈ 0.25): their judgment capacity is genuinely necessary, but institutional structures amplify their positional advantage beyond what the coordination function requires. LLM-assisted synthesizers are targets (d ≈ 0.75): they bear the cost of having valid frameworks dismissed due to lack of institutional credentials, even when mechanistic coherence and component evidence are strong. Individual decision-makers are full targets (d ≈ 0.95): trapped between institutional caution and synthesis proliferation with no reliable filter, bearing costs of both error types. Funding bodies are mild beneficiaries (d ≈ 0.3): validation signals simplify their allocation decisions. Methodologists are analytical observers (d ≈ 0.5): studying the structure without being positioned by it.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents mislabeling institutional rent-seeking as pure epistemic necessity. The genuine coordination function—expert judgment distinguishing valid from invalid synthesis—is real and irreducible given current AI capabilities. But institutional structures have layered extraction onto this function: credentialing barriers that exceed what validation requires, peer review delays that serve gatekeeping rather than quality control, journal hierarchies that concentrate legitimacy beyond what mechanistic assessment needs. The mandatrophy resolution distinguishes the mountain (validation judgment is structurally necessary) from the tangled rope (institutional capture of validation pathways extracts rents from synthesizers while coordinating quality control). As LLM capabilities advance, the mountain may erode—if synthesis tools develop reliable self-validation, the coordination function dissolves and only the extraction remains.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    llm_validation_capability_trajectory,
    'Will LLMs develop reliable self-validation capabilities that can distinguish mechanistically valid from merely coherent syntheses without human expert judgment?',
    'Empirical testing of LLM accuracy rates on domain expert-validated mechanistic claims over time; measurement of false positive rates for plausible but incorrect frameworks; comparison of LLM self-assessment accuracy against expert judgment.',
    'If LLMs achieve reliable self-validation, the mountain dissolves—synthesis and validation collapse into a single automated function, eliminating the coordination justification for institutional gatekeeping. If validation remains human-dependent, the mountain persists and institutional structures retain their coordination function (though extraction may continue to accumulate).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(llm_validation_capability_trajectory, empirical, 'Whether validation judgment remains irreducibly human or becomes automatable').

omega_variable(
    institutional_vs_mechanistic_validation,
    'Is institutional validation (peer review, credentialing, journal publication) structurally necessary for distinguishing valid syntheses, or could mechanistic coherence testing and component evidence aggregation provide equivalent protection against false positives?',
    'Natural experiments comparing accuracy rates of institutionally validated claims versus mechanistically validated claims in domains where both pathways exist; measurement of false positive and false negative rates for each validation mode.',
    'If mechanistic validation proves equivalent, institutional gatekeeping is pure extraction riding on a coordination function that could be provided otherwise. If institutional validation is structurally superior, the extraction is the unavoidable cost of the coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_vs_mechanistic_validation, empirical, 'Whether institutional structures provide validation quality beyond mechanistic assessment').

omega_variable(
    false_summit_institutional_capture,
    'Is the validation-judgment separation a genuine epistemic mountain (synthesis tools cannot self-validate regardless of institutional arrangements) or a false summit (institutional structures present as natural what is actually constructed gatekeeping)?',
    'Decomposition into component constraints: (1) the computational constraint that LLMs cannot reliably self-validate mechanistic claims, (2) the institutional constraint that validation pathways require credentialing and peer review. Test whether (1) persists in contexts where (2) is absent or weakened.',
    'If the separation is a false summit, the constraint should decompose into a genuine mountain (LLM validation limits) and a tangled rope or snare (institutional gatekeeping). If it is a true mountain, the separation persists across all institutional arrangements.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_institutional_capture, conceptual, 'Whether beneficiary presence indicates false summit or unavoidable coordination cost').

omega_variable(
    error_type_tradeoff_framing,
    'Is the institutional emphasis on minimizing false positives (Type I errors) versus the pragmatic emphasis on minimizing false negatives (Type II errors) a difference in epistemic standards or a difference in risk allocation preferences?',
    'Analysis of whether the two framings can be reconciled through explicit risk-benefit calculation, or whether they represent incompatible value commitments about who should bear uncertainty costs.',
    'If the difference is purely epistemic, one reading is correct and the others are false summits. If the difference is value-based, the readings represent genuine alternative commitments and the kernel is contested rather than natural.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(error_type_tradeoff_framing, conceptual, 'Whether validation standards reflect epistemic necessity or risk preference').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(validation_judgment_separation, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vali_tr_t0, validation_judgment_separation, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(vali_tr_t0, observed).
narrative_ontology:measurement(vali_tr_t5, validation_judgment_separation, theater_ratio, 5, 0.12).
narrative_ontology:measurement_basis(vali_tr_t5, observed).
narrative_ontology:measurement(vali_tr_t10, validation_judgment_separation, theater_ratio, 10, 0.13).
narrative_ontology:measurement_basis(vali_tr_t10, observed).
narrative_ontology:measurement(vali_tr_t15, validation_judgment_separation, theater_ratio, 15, 0.14).
narrative_ontology:measurement_basis(vali_tr_t15, observed).
narrative_ontology:measurement(vali_tr_t20, validation_judgment_separation, theater_ratio, 20, 0.145).
narrative_ontology:measurement_basis(vali_tr_t20, observed).
narrative_ontology:measurement(vali_tr_t25, validation_judgment_separation, theater_ratio, 25, 0.15).
narrative_ontology:measurement_basis(vali_tr_t25, projected).

% Extraction over time
narrative_ontology:measurement(vali_be_t0, validation_judgment_separation, base_extractiveness, 0, 0.08).
narrative_ontology:measurement_basis(vali_be_t0, observed).
narrative_ontology:measurement(vali_be_t5, validation_judgment_separation, base_extractiveness, 5, 0.09).
narrative_ontology:measurement_basis(vali_be_t5, observed).
narrative_ontology:measurement(vali_be_t10, validation_judgment_separation, base_extractiveness, 10, 0.1).
narrative_ontology:measurement_basis(vali_be_t10, observed).
narrative_ontology:measurement(vali_be_t15, validation_judgment_separation, base_extractiveness, 15, 0.11).
narrative_ontology:measurement_basis(vali_be_t15, observed).
narrative_ontology:measurement(vali_be_t20, validation_judgment_separation, base_extractiveness, 20, 0.115).
narrative_ontology:measurement_basis(vali_be_t20, observed).
narrative_ontology:measurement(vali_be_t25, validation_judgment_separation, base_extractiveness, 25, 0.12).
narrative_ontology:measurement_basis(vali_be_t25, projected).

% Suppression requirement over time
narrative_ontology:measurement(vali_su_t0, validation_judgment_separation, suppression_requirement, 0, 0.06).
narrative_ontology:measurement_basis(vali_su_t0, observed).
narrative_ontology:measurement(vali_su_t5, validation_judgment_separation, suppression_requirement, 5, 0.065).
narrative_ontology:measurement_basis(vali_su_t5, observed).
narrative_ontology:measurement(vali_su_t10, validation_judgment_separation, suppression_requirement, 10, 0.07).
narrative_ontology:measurement_basis(vali_su_t10, observed).
narrative_ontology:measurement(vali_su_t15, validation_judgment_separation, suppression_requirement, 15, 0.073).
narrative_ontology:measurement_basis(vali_su_t15, observed).
narrative_ontology:measurement(vali_su_t20, validation_judgment_separation, suppression_requirement, 20, 0.076).
narrative_ontology:measurement_basis(vali_su_t20, observed).
narrative_ontology:measurement(vali_su_t25, validation_judgment_separation, suppression_requirement, 25, 0.08).
narrative_ontology:measurement_basis(vali_su_t25, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(validation_judgment_separation, information_standard).

% DUAL FORMULATION NOTE:
% This constraint is downstream of llm_synthesis_capacity (the upstream rope that collapsed synthesis friction). The validation-judgment separation is what remains after synthesis becomes cheap: the irreducible need for expert judgment to distinguish valid from merely coherent frameworks. The two constraints form a family: synthesis capacity (rope, low extraction) enables the production of plausible claims; validation judgment (mountain with FSM characteristics, modest extraction) filters them. The upstream constraint's success makes the downstream constraint's institutional capture more visible.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
