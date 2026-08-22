% ============================================================================
% CONSTRAINT STORY: humane_treatment_standard__contextual_necessity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_humane_treatment_standard__contextual_necessity, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: humane_treatment_standard__contextual_necessity
 *   human_readable: Common Article 3 with Contextual Necessity Override
 *   domain: international_humanitarian_law/state_security
 *
 * SUMMARY:
 *   Common Article 3 of the Geneva Conventions establishes baseline
 *   humanitarian protections for detainees: food, shelter, medical care, and
 *   prohibition of torture and degrading treatment. This constraint
 *   instantiates ONE reading of the contested 'humane treatment standard'
 *   kernel: the contextual necessity reading, which permits security agencies
 *   to override baseline protections when they assert that national security
 *   imperatives (extracting intelligence from high-value detainees,
 *   preventing imminent attacks) take precedence. Under this reading, 'humane
 *   treatment' becomes conditional—its standards shift based on the detaining
 *   agency's operational assessment. This contrasts sharply with the absolute
 *   prohibition reading (no override permitted, ever) and the proportionality
 *   balancing reading (override permitted only if balanced against detainee
 *   dignity). The reading structures authority asymmetrically: the agency
 *   that detains also judges necessity, defines what counts as humane under
 *   necessity, and records the interrogation—a self-judging framework with
 *   structural bias toward operational flexibility.
 *
 * KEY AGENTS:
 *   - Security agencies: institutional agenda-setters who define necessity, conduct enhanced interrogation, classify detainees as high-value. Power: institutional; Exit: analytical (no operational exit from their role); Directionality: near beneficiary (they control the standard, collect intelligence, face minimal accountability for necessity claims).
 *   - High-value detainees: powerless targets, subject to enhanced interrogation when so classified. Power: powerless; Exit: trapped (captivity, interrogation pressure); Directionality: near full target (protections suspended, subjected to techniques prohibited under baseline).
 *   - International humanitarian law bodies: institutional beneficiaries who maintain legal form (Common Article 3 formally binding) while gaining plausible deniability about enforcement. Power: institutional; Exit: analytical; Directionality: near beneficiary (preserve legitimacy without enforcement conflict).
 *   - Detainee advocates and human rights monitors: excluded during the interrogation window. Power: moderate; Exit: constrained (access denied, information classified); Directionality: neither benefit nor pay directly, but would object loudly if present.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(humane_treatment_standard__contextual_necessity, 0.68).
domain_priors:suppression_score(humane_treatment_standard__contextual_necessity, 0.79).
domain_priors:theater_ratio(humane_treatment_standard__contextual_necessity, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(humane_treatment_standard__contextual_necessity, extractiveness, 0.68).
narrative_ontology:constraint_metric(humane_treatment_standard__contextual_necessity, suppression_requirement, 0.79).
narrative_ontology:constraint_metric(humane_treatment_standard__contextual_necessity, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(humane_treatment_standard__contextual_necessity, accessibility_collapse, 0.61).
narrative_ontology:constraint_metric(humane_treatment_standard__contextual_necessity, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(humane_treatment_standard__contextual_necessity, tangled_rope).
narrative_ontology:human_readable(humane_treatment_standard__contextual_necessity, "Common Article 3 with Contextual Necessity Override").
narrative_ontology:topic_domain(humane_treatment_standard__contextual_necessity, "international_humanitarian_law/state_security").

domain_priors:requires_active_enforcement(humane_treatment_standard__contextual_necessity).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(humane_treatment_standard__contextual_necessity, '89248a54-cc96-4d40-877d-915cad58028d').
narrative_ontology:cs_kernel_codification('89248a54-cc96-4d40-877d-915cad58028d', fixed_text).
narrative_ontology:cs_authority_grounding('89248a54-cc96-4d40-877d-915cad58028d', extraction).
narrative_ontology:cs_interpretation_layer_present('89248a54-cc96-4d40-877d-915cad58028d').
narrative_ontology:cs_reading_relation('89248a54-cc96-4d40-877d-915cad58028d', humane_treatment_standard__absolute_prohibition, forecloses).
narrative_ontology:cs_reading_relation('89248a54-cc96-4d40-877d-915cad58028d', humane_treatment_standard__proportionality_balancing, influences).
narrative_ontology:cs_axiom('89248a54-cc96-4d40-877d-915cad58028d', foundational, necessity_override_legitimate_when_asserted_by_security_agency).
narrative_ontology:cs_axiom_status(necessity_override_legitimate_when_asserted_by_security_agency, holdable).
narrative_ontology:cs_axiom_grounding('89248a54-cc96-4d40-877d-915cad58028d', necessity_override_legitimate_when_asserted_by_security_agency, conventional).
narrative_ontology:cs_axiom('89248a54-cc96-4d40-877d-915cad58028d', foundational, detainee_dignity_conditional_on_security_imperatives).
narrative_ontology:cs_axiom_status(detainee_dignity_conditional_on_security_imperatives, holdable).
narrative_ontology:cs_axiom_grounding('89248a54-cc96-4d40-877d-915cad58028d', detainee_dignity_conditional_on_security_imperatives, deontological).
narrative_ontology:cs_reference_frame('89248a54-cc96-4d40-877d-915cad58028d', article_three_baseline_non_derogable).
narrative_ontology:cs_drift_state('89248a54-cc96-4d40-877d-915cad58028d', post_war_on_terror_security_expansion, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('89248a54-cc96-4d40-877d-915cad58028d', '').
narrative_ontology:cs_kernel_id(humane_treatment_standard__contextual_necessity, humane_treatment_standard).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(humane_treatment_standard__contextual_necessity, security_agencies).
narrative_ontology:constraint_beneficiary(humane_treatment_standard__contextual_necessity, state_apparatus).
narrative_ontology:constraint_victim(humane_treatment_standard__contextual_necessity, high_value_detainees).
narrative_ontology:constraint_victim(humane_treatment_standard__contextual_necessity, non_combatant_bystanders_in_security_operations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(humane_treatment_standard__contextual_necessity, international_humanitarian_law_bodies).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Define 'humane treatment' in operational contexts, decide when necessity overrides baseline standards, and conduct enhanced interrogation under the framework's discretion. Their institutional mandate is threat prevention; under this reading they author the necessity judgment that suspends protections. They face minimal accountability for necessity determinations in real-time operations.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__contextual_necessity, security_agencies, agenda_setter,
    institutional, generational, analytical, national).

% Subjected to enhanced interrogation (sleep deprivation, stress positions, simulated drowning, psychological pressure) when classified as high-value threats. Common Article 3 nominally applies, but this reading permits its suspension when the detaining agency asserts operational necessity. Their only exit is through interrogation compliance or legal challenge during/after detention, both constrained by information asymmetry and state control of evidence.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__contextual_necessity, high_value_detainees, payer,
    powerless, immediate, trapped, local).

% Caught in expanded detention net under expanded necessity definitions. As 'associated' persons or in proximity to suspected high-value targets, they are detained and subject to standards that shift based on the detaining agency's assessment. Their exit depends on being cleared by the same agency that detained them.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__contextual_necessity, non_combatant_bystanders_in_security_operations, payer,
    moderate, biographical, constrained, regional).

% Can maintain a formal Common Article 3 framework while permitting operational flexibility via necessity clauses. This reading allows them to preserve institutional legitimacy (a binding treaty exists) while deferring judgment on its application to state actors. They collect institutional authority from the preservation of the legal form while gaining plausible deniability about enforcement.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__contextual_necessity, international_humanitarian_law_bodies, beneficiary,
    institutional, generational, analytical, global).

% Retains operational discretion to override baseline protections without formally withdrawing from Common Article 3. The constraint operates as authorization-in-reserve: the treaty remains binding in form, but its application is conditional on the state's security judgment. The state benefits from appearing bound while exercising effective unilateral suspension authority.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__contextual_necessity, state_apparatus, beneficiary,
    institutional, generational, analytical, national).

% Human rights organizations and legal advocates are systematically excluded from access to detainees during the enhanced-interrogation window. They would contest the necessity judgment and demand baseline application; their exclusion is enforced through classification, isolation, and state monopoly on detention intelligence.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__contextual_necessity, detainee_advocates, excluded,
    moderate, biographical, constrained, global).

% Red Cross, UN human rights mechanisms, and treaty monitoring bodies face information asymmetry: necessity determinations are classified, detainee access is restricted, and investigative authority is limited. They observe and document violations post-facto but cannot prevent real-time discretionary application of the override.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__contextual_necessity, international_oversight_bodies, observer,
    organized, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(humane_treatment_standard__contextual_necessity, security_agencies).
narrative_ontology:fixing_cost_class(humane_treatment_standard__contextual_necessity, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Common Article 3 coordinates a baseline humanitarian standard for treatment of detainees across armed conflict contexts—food, shelter, medical care, protection from torture and degrading treatment. The contextual necessity reading preserves this baseline while permitting security agencies to override it when they assess that national security imperatives demand enhanced interrogation.
% TRANSFER_FUNCTION: Transfers authority to define 'humane treatment' from an external standard (treaty baseline) to the detaining agency's operational judgment. In necessity scenarios, it moves detainee protection status from inalienable (under absolute prohibition reading) to conditional (under this reading). It also transfers interrogation techniques from prohibited to permitted, contingent on agency necessity assertion.
% ABSENT_VOICES: Detainees themselves cannot testify about necessity during enhanced interrogation or assert that necessity was falsely claimed (information asymmetry forbids it). Human rights monitors and international oversight bodies are excluded from access during the critical interrogation window. Legal counsel for detainees is restricted or absent. The detainees' own assessment of whether interrogation is 'humane' is structurally irrelevant to the necessity determination.
% DISAPPEARANCE_RATIONALE: If this reading and its necessity override vanished—if the contextual necessity permissive interpretation were replaced by absolute prohibition—security agencies would lose operational flexibility in high-value detainee handling. States would face legal exposure for enhanced techniques currently defended as necessary; interrogation practices would revert to baseline Common Article 3 methods. The apparatus of necessity-justified override is institutionally embedded; its disappearance would force transparency and accountability on current detention practices.
% FOUNDING_PROBLEM: How can international humanitarian law protect detainee dignity while preserving state capacity to extract time-critical intelligence in existential security scenarios? Common Article 3 was written without explicit necessity exception; the contextual necessity reading resolves this by permitting operational override when agencies deem security imperatives override treaty baseline.
% FOUNDING_PROBLEM_CORROBORATION: Security agencies and several states assert the founding problem is live and acute: that rigid adherence to baseline standards in high-value detainee interrogation blinds the state to threats. However, human rights bodies, independent legal scholars, and states with absolute-prohibition policies attest that the founding problem is a fabricated urgency—that effective interrogation does not require techniques beyond baseline standards, and that necessity claims are routinely used to justify systematic abuse. The empirical question (do enhanced techniques yield intelligence unavailable through baseline methods?) remains contested; the claim is not corroborated by credible sources outside the security apparatus asserting the necessity.
narrative_ontology:disappearance_verdict(humane_treatment_standard__contextual_necessity, world_rearranges).
narrative_ontology:founding_problem_status(humane_treatment_standard__contextual_necessity, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(humane_treatment_standard__contextual_necessity, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(humane_treatment_standard__contextual_necessity, 'none', 1).
narrative_ontology:epsilon_provenance(humane_treatment_standard__contextual_necessity, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(humane_treatment_standard__contextual_necessity_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(humane_treatment_standard__contextual_necessity, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(humane_treatment_standard__contextual_necessity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.68) and climbing over the interval because the constraint progressively widens the scope of 'necessity' determinations and lengthens the window during which overrides apply. Early in the interval (t=0) the claim is that necessity applies only to genuine high-value targets in existential scenarios; by t=25 the necessity concept has expanded to cover expanded classes of 'associated' persons and lower-threshold scenarios. Suppression is higher still (0.79) because the constraint's persistence depends on: (1) classification secrecy (what counts as high-value is classified), (2) interrogation documentation control (agencies control records), (3) access denial (monitors cannot observe interrogations), and (4) necessity-claim immunity (agencies cannot be held accountable for necessity assertions in real-time). Theater ratio is moderate (0.42): the baseline Common Article 3 protections are real and visible (the constraint does provide actual food, shelter, medical care when baseline applies), but a growing share of the constraint's operation is performative—maintaining the appearance of humanitarian compliance while sheltering enhanced interrogation from scrutiny. The three-metric series share one time grid (every metric measured at every time point) to enable temporal comparison.
 *
 * PERSPECTIVAL GAP:
 *   The security agency seat and the detainee seat should compute dramatically differently. From the agency's perspective, this reading is coordination: it permits operationally necessary interrogation while maintaining formal treaty compliance—a workable balance between humanitarian form and security substance. From the detainee's perspective, the same structure is pure extraction: protections nominally guaranteed by Common Article 3 are conditional on the detaining agency's judgment, which is rendered in secret, cannot be appealed during interrogation, and is structured to favor operational over humanitarian interpretation. The agency collects from the constraint (intelligence, operational flexibility, institutional discretion); the detainee bears costs (interrogation, uncertainty about which protections apply, no ability to contest necessity claims). The engine computes this divergence from the power/exit/beneficiary/victim data; the authored claim (tangled rope) reflects the structural asymmetry, not a reconciliation of the seats.
 *
 * DIRECTIONALITY LOGIC:
 *   Security agencies are the structural beneficiaries: they define necessity, conduct enhanced interrogation, and face minimal accountability for necessity determinations. Their exit is analytical (they do not exit the role; their position is structurally secure). Directionality near 0.1–0.2 (modest beneficiary). High-value detainees are the structural targets: they are subject to enhanced interrogation, their protections are suspended, and their exit is trapped (held in custody, pressure to confess/cooperate). Directionality near 0.85–0.95 (full target). International oversight bodies are secondary beneficiaries: they preserve institutional legitimacy by maintaining the treaty form while deferring to state security judgments, avoiding direct conflict with powerful states. Their directionality is near 0.15–0.25 (modest beneficiary, but through institutional alignment rather than extraction). Non-combatant detainees face expanded necessity definitions that pull them into the enhanced-interrogation category; their directionality is near 0.80–0.90 (near-target, trapped exit).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (how to extract intelligence while preserving humanitarian standards) was originally posed as a genuine coordination challenge. However, over the interval, the empirical evidence increasingly shows no trade-off exists: interrogation under baseline Article 3 methods yields equivalent intelligence to enhanced techniques, and enhanced techniques introduce systematic errors and compliance costs. Despite this evidence, the constraint persists and expands (extractiveness and suppression both climbing, theater rising). This is mandatrophy: the founding problem is functionally dead (baseline methods work), but the institutional apparatus persists because agencies benefit from the necessity discretion independently of whether necessity is empirically real. The constraint no longer solves the problem it was built for; it persists as pure extraction (security agencies collect discretion and operational flexibility) riding on the ghost of coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    necessity_claim_empirical_truth,
    'Do enhanced interrogation techniques actually yield intelligence unavailable through baseline Common Article 3 methods, or is the necessity claim empirically false?',
    'Systematic review of interrogation outcome data (detainee intelligence quality, actionability, accuracy) controlled for method. Comparison of intelligence yield from high-value detainees interrogated under baseline vs. enhanced methods in matched scenarios. Independent analysis by intelligence agencies not claiming necessity authority.',
    'If enhanced techniques do NOT yield superior intelligence, the founding problem is dead and the constraint is pure extraction (mandatrophy). If they do yield superior intelligence, the tradeoff is real and the constraint remains coordination with extraction overhead. If the question is empirically undecidable (classified data, no controls, outcome measures conflated), the necessity claim is irrefutable by design and functions as infinite authority.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(necessity_claim_empirical_truth, empirical, 'Empirical basis for necessity claim.').

omega_variable(
    necessity_vs_pretense_asymmetry,
    'Is the necessity override genuinely limited to high-value targets and existential scenarios, or is it routinely invoked for lower-threshold detainees and preventive intelligence?',
    'Declassified interrogation records; FOIA release of necessity justifications; independent investigation by international oversight bodies with access to detainee intake and classification records.',
    'If necessity is actually constrained to genuine high-value/existential cases, the constraint operates near the tangled rope boundary (real coordination with controlled override). If necessity is routinely invoked for lower-threshold cases, extractiveness reclassifies toward snare (pure extraction using necessity as cover). If the question cannot be answered due to classification, the opacity itself is suppression and feeds the high theater ratio.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(necessity_vs_pretense_asymmetry, empirical, 'Whether necessity invocation is actually bounded or systematically inflated.').

omega_variable(
    agency_necessity_accountability,
    'Can a detainee or international monitor challenge the agency''s necessity determination during interrogation, or is the determination unilaterally final?',
    'Legal review of appeal mechanisms available to detainees undergoing enhanced interrogation. Analysis of whether any successful challenges to necessity determinations have occurred in practice.',
    'If detainees can contest necessity in real-time, the constraint has an external check and directionality becomes less asymmetric. If the determination is unilaterally final (no appeal until post-interrogation, no external review during interrogation), the structural power imbalance is confirmed and directionality is at the target extreme for detainees.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(agency_necessity_accountability, empirical, 'Whether necessity determination is appealable or final.').

omega_variable(
    suppression_internalization_after_release,
    'To what extent does suppression persist as internalized belief after release (detainee no longer in state custody, structural suppression no longer operative)?',
    'Longitudinal interview with released detainees about agency distrust, interrogation trauma, residual belief in interrogation effectiveness, willingness to contest official narratives.',
    'If suppression is purely structural and dissolves upon release, the constraint''s reach is limited to detainees in custody. If suppression is substantially internalized (detainees continue to believe resistance is futile, continue to distrust agencies, continue to avoid challenging necessity narratives), the constraint''s effective suppression extends beyond custody and the psychological cost persists long-term.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization_after_release, empirical, 'Internalized vs. structural components of suppression.').

omega_variable(
    alternative_reading_coexistence_condition,
    'Within a single state apparatus, can the contextual necessity reading and the absolute prohibition reading coexist, or do they logically require different institutional arrangements?',
    'Formal analysis of whether a single legal framework can hold ''baseline protections are non-derogable'' and ''baseline protections are overridable under necessity'' as simultaneous valid claims. Review of state practice: do states that endorse contextual necessity also maintain coherent absolute prohibition rules for other classes of detainees?',
    'If coexistence is logically impossible (the axioms directly contradict), contextual necessity forecloses absolute prohibition. If coexistence is possible (through scope differentiation, tiering, or institutional separation), they merely compete and do not foreclose—both readings remain live options for different parties. This affects the reading_relations value in cs_structure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_reading_coexistence_condition, conceptual, 'Whether the contextual necessity and absolute prohibition readings logically foreclose each other or merely compete.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(humane_treatment_standard__contextual_necessity, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(huma_tr_t0, humane_treatment_standard__contextual_necessity, theater_ratio, 0, 0.28).
narrative_ontology:measurement(huma_tr_t5, humane_treatment_standard__contextual_necessity, theater_ratio, 5, 0.31).
narrative_ontology:measurement(huma_tr_t10, humane_treatment_standard__contextual_necessity, theater_ratio, 10, 0.35).
narrative_ontology:measurement(huma_tr_t15, humane_treatment_standard__contextual_necessity, theater_ratio, 15, 0.39).
narrative_ontology:measurement(huma_tr_t20, humane_treatment_standard__contextual_necessity, theater_ratio, 20, 0.41).
narrative_ontology:measurement(huma_tr_t25, humane_treatment_standard__contextual_necessity, theater_ratio, 25, 0.42).

% Extraction over time
narrative_ontology:measurement(huma_be_t0, humane_treatment_standard__contextual_necessity, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(huma_be_t5, humane_treatment_standard__contextual_necessity, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(huma_be_t10, humane_treatment_standard__contextual_necessity, base_extractiveness, 10, 0.61).
narrative_ontology:measurement(huma_be_t15, humane_treatment_standard__contextual_necessity, base_extractiveness, 15, 0.65).
narrative_ontology:measurement(huma_be_t20, humane_treatment_standard__contextual_necessity, base_extractiveness, 20, 0.67).
narrative_ontology:measurement(huma_be_t25, humane_treatment_standard__contextual_necessity, base_extractiveness, 25, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(huma_su_t0, humane_treatment_standard__contextual_necessity, suppression_requirement, 0, 0.68).
narrative_ontology:measurement(huma_su_t5, humane_treatment_standard__contextual_necessity, suppression_requirement, 5, 0.72).
narrative_ontology:measurement(huma_su_t10, humane_treatment_standard__contextual_necessity, suppression_requirement, 10, 0.75).
narrative_ontology:measurement(huma_su_t15, humane_treatment_standard__contextual_necessity, suppression_requirement, 15, 0.77).
narrative_ontology:measurement(huma_su_t20, humane_treatment_standard__contextual_necessity, suppression_requirement, 20, 0.78).
narrative_ontology:measurement(huma_su_t25, humane_treatment_standard__contextual_necessity, suppression_requirement, 25, 0.79).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(humane_treatment_standard__contextual_necessity, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(humane_treatment_standard__contextual_necessity, 0.12).
narrative_ontology:affects_constraint(humane_treatment_standard__contextual_necessity, humane_treatment_standard__absolute_prohibition).
narrative_ontology:affects_constraint(humane_treatment_standard__contextual_necessity, humane_treatment_standard__proportionality_balancing).
narrative_ontology:affects_constraint(humane_treatment_standard__contextual_necessity, state_security_exemption_doctrine).
narrative_ontology:affects_constraint(humane_treatment_standard__contextual_necessity, intelligence_interrogation_efficacy_empirical_claim).

% DUAL FORMULATION NOTE:
% This constraint is part of the 'humane_treatment_standard' kernel constraint family. The kernel (Common Article 3 baseline protections for detainees) is contested across three readings: absolute_prohibition (non-derogable floor), contextual_necessity (this file; agency discretion override), and proportionality_balancing (external weighing process). Each reading instantiates a different ε, victim set, and beneficiary structure. The readings do not represent different perspectives on the same constraint; they are structurally distinct constraints instantiated by the same formal rule (Common Article 3) under different interpretations. The ε values differ because the readings place authority differently: absolute prohibition treats any override as null and void (low ε for detainee protection); contextual necessity permits agency override (high ε for detainee protection, extracted to agency benefit); proportionality balancing requires proportional justification (medium ε, case-dependent). Network links document the family relationships and downstream effects (the empirical interrogation efficacy claim directly undermines the necessity justification for this reading's override).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(humane_treatment_standard__contextual_necessity, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
