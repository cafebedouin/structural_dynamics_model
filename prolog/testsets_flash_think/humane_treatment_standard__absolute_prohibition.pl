% ============================================================================
% CONSTRAINT STORY: humane_treatment_standard__absolute_prohibition
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_humane_treatment_standard__absolute_prohibition, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: humane_treatment_standard__absolute_prohibition
 *   human_readable: Absolute Prohibition of Torture and Degrading Treatment (Common Article 3 Reading)
 *   domain: international_humanitarian_law/human_rights
 *
 * SUMMARY:
 *   This constraint story instantiates the 'absolute_prohibition' reading of
 *   the 'humane_treatment_standard' kernel, as enshrined in Common Article 3
 *   of the Geneva Conventions. This reading asserts that no circumstances,
 *   including armed conflict or national security imperatives, permit torture
 *   or degrading treatment of detainees. It establishes non-derogable minimum
 *   standards for all persons deprived of liberty, aiming to eliminate such
 *   practices entirely. The constraint is claimed as a 'rope' from the
 *   perspective of its ideal function (coordinating states around a universal
 *   moral baseline), but its operational metrics reflect the substantial
 *   extraction it imposes on states wishing to use coercive methods, and the
 *   high suppression required to maintain it against persistent resistance.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(humane_treatment_standard__absolute_prohibition, 0.85).
domain_priors:suppression_score(humane_treatment_standard__absolute_prohibition, 0.9).
domain_priors:theater_ratio(humane_treatment_standard__absolute_prohibition, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(humane_treatment_standard__absolute_prohibition, extractiveness, 0.85).
narrative_ontology:constraint_metric(humane_treatment_standard__absolute_prohibition, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(humane_treatment_standard__absolute_prohibition, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(humane_treatment_standard__absolute_prohibition, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(humane_treatment_standard__absolute_prohibition, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(humane_treatment_standard__absolute_prohibition, rope).
narrative_ontology:human_readable(humane_treatment_standard__absolute_prohibition, "Absolute Prohibition of Torture and Degrading Treatment (Common Article 3 Reading)").
narrative_ontology:topic_domain(humane_treatment_standard__absolute_prohibition, "international_humanitarian_law/human_rights").

domain_priors:requires_active_enforcement(humane_treatment_standard__absolute_prohibition).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(humane_treatment_standard__absolute_prohibition, 'c2ec0cd8-e0c0-4823-a8c2-b9fc53dbec7f').
narrative_ontology:cs_kernel_codification('c2ec0cd8-e0c0-4823-a8c2-b9fc53dbec7f', fixed_text).
narrative_ontology:cs_authority_grounding('c2ec0cd8-e0c0-4823-a8c2-b9fc53dbec7f', lineage).
narrative_ontology:cs_interpretation_layer_present('c2ec0cd8-e0c0-4823-a8c2-b9fc53dbec7f').
narrative_ontology:cs_reading_relation('c2ec0cd8-e0c0-4823-a8c2-b9fc53dbec7f', humane_treatment_standard__contextual_necessity, forecloses).
narrative_ontology:cs_reading_relation('c2ec0cd8-e0c0-4823-a8c2-b9fc53dbec7f', humane_treatment_standard__proportionality_balancing, forecloses).
narrative_ontology:cs_axiom('c2ec0cd8-e0c0-4823-a8c2-b9fc53dbec7f', foundational, torture_categorically_impermissible).
narrative_ontology:cs_axiom_status(torture_categorically_impermissible, holdable).
narrative_ontology:cs_axiom_grounding('c2ec0cd8-e0c0-4823-a8c2-b9fc53dbec7f', torture_categorically_impermissible, deontological).
narrative_ontology:cs_axiom('c2ec0cd8-e0c0-4823-a8c2-b9fc53dbec7f', foundational, human_dignity_non_derogable).
narrative_ontology:cs_axiom_status(human_dignity_non_derogable, holdable).
narrative_ontology:cs_axiom_grounding('c2ec0cd8-e0c0-4823-a8c2-b9fc53dbec7f', human_dignity_non_derogable, deontological).
narrative_ontology:cs_reference_frame('c2ec0cd8-e0c0-4823-a8c2-b9fc53dbec7f', universal_human_dignity_framework).
narrative_ontology:cs_drift_state('c2ec0cd8-e0c0-4823-a8c2-b9fc53dbec7f', contemporary_security_paradigm, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('c2ec0cd8-e0c0-4823-a8c2-b9fc53dbec7f', '').
narrative_ontology:cs_kernel_id(humane_treatment_standard__absolute_prohibition, humane_treatment_standard).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(humane_treatment_standard__absolute_prohibition, detainees).
narrative_ontology:constraint_beneficiary(humane_treatment_standard__absolute_prohibition, human_rights_advocates).
narrative_ontology:constraint_beneficiary(humane_treatment_standard__absolute_prohibition, international_legal_system).
narrative_ontology:constraint_victim(humane_treatment_standard__absolute_prohibition, state_security_agencies).
narrative_ontology:constraint_victim(humane_treatment_standard__absolute_prohibition, interrogators).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(humane_treatment_standard__absolute_prohibition, states_claiming_security_exceptions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals held in armed conflict, whose fundamental right to humane treatment is protected by this standard. They are entirely dependent on its enforcement for their safety and dignity.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__absolute_prohibition, detainees, beneficiary,
    powerless, immediate, trapped, global).

% Organizations and individuals who champion the absolute prohibition, monitor compliance, and advocate for its strict enforcement. They benefit from the clarity and moral force of the standard.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__absolute_prohibition, human_rights_advocates, beneficiary,
    organized, generational, analytical, global).

% The framework of treaties, courts, and norms that establishes, interprets, and seeks to enforce Common Article 3. It sets the standard and provides mechanisms for accountability, though often with limited direct coercive power.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__absolute_prohibition, international_legal_system, agenda_setter,
    institutional, civilizational, analytical, universal).

% Government bodies (intelligence, military, police) responsible for national security and interrogation. This constraint restricts their methods, imposing costs in terms of perceived operational effectiveness and requiring adherence to strict legal boundaries.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__absolute_prohibition, state_security_agencies, payer,
    institutional, biographical, constrained, national).

% Individual agents directly involved in questioning detainees. The absolute prohibition limits their toolkit, potentially increasing the difficulty or time required to extract information, and exposing them to legal risk for violations.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__absolute_prohibition, interrogators, payer,
    moderate, immediate, constrained, local).

% States that, while signatories to international law, argue for or implement interpretations that permit 'enhanced interrogation' or other practices that cross the absolute prohibition threshold, often citing national security imperatives. They bear the cost of international condemnation and legal challenges.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__absolute_prohibition, states_claiming_security_exceptions, agenda_setter,
    powerful, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(humane_treatment_standard__absolute_prohibition, states_claiming_security_exceptions, payer).

% Academics, policymakers, and legal scholars who argue that humane treatment standards, while important, must be balanced against national security needs and can be context-dependent. Their views are fundamentally incompatible with the absolute prohibition reading.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__absolute_prohibition, contextual_necessity_proponents, excluded,
    organized, biographical, analytical, global).

% Those who advocate for a legal framework that balances detainee dignity against security needs, allowing for a proportional response rather than an absolute ban. This position is foreclosed by the absolute prohibition reading.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__absolute_prohibition, proportionality_balancing_proponents, excluded,
    organized, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a universal, non-derogable baseline for humane treatment of persons deprived of liberty during armed conflict, aiming to prevent a 'race to the bottom' in detainee conditions and ensure a common standard of human dignity.
% TRANSFER_FUNCTION: Transfers the right to humane treatment to all detainees, regardless of their status, and imposes a non-derogable obligation on states to uphold this standard, thereby restricting state power in interrogation and detention practices.
% ABSENT_VOICES: Proponents of 'contextual necessity' or 'proportionality balancing' for interrogation methods are structurally excluded from the normative framework of absolute prohibition. They would argue for flexibility in extreme security situations, but this reading permits no such exceptions.
% DISAPPEARANCE_RATIONALE: If the absolute prohibition vanished overnight, states would likely revert to more coercive and inhumane interrogation methods, detainee treatment standards would plummet globally, and the foundational principles of international human rights and humanitarian law would be catastrophically undermined, leading to a significant reorganization of state practice and international relations.
% FOUNDING_PROBLEM: The widespread atrocities, torture, and inhumane treatment of prisoners and civilians during armed conflicts, particularly World War II, which highlighted the urgent need for universal, non-derogable standards to protect human dignity in all circumstances.
% FOUNDING_PROBLEM_CORROBORATION: International human rights organizations (e.g., Amnesty International, Human Rights Watch), UN special rapporteurs, and the International Committee of the Red Cross (ICRC) consistently document ongoing violations and the persistent threat of inhumane treatment, corroborating that the founding problem remains live and the standard is continuously challenged.
narrative_ontology:disappearance_verdict(humane_treatment_standard__absolute_prohibition, world_rearranges).
narrative_ontology:founding_problem_status(humane_treatment_standard__absolute_prohibition, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(humane_treatment_standard__absolute_prohibition, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(humane_treatment_standard__absolute_prohibition, 'none', 1).
narrative_ontology:epsilon_provenance(humane_treatment_standard__absolute_prohibition, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(humane_treatment_standard__absolute_prohibition_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(humane_treatment_standard__absolute_prohibition, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(humane_treatment_standard__absolute_prohibition_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The `extractiveness` is high (0.85) because this reading demands a complete cessation of practices that states might find 'useful' for security, imposing significant costs on state security agencies. `Suppression` is very high (0.90) as it actively seeks to eliminate all alternatives (torture, degrading treatment) and requires continuous enforcement against state resistance. `Theater_ratio` is moderate (0.45), reflecting that while many states publicly affirm the prohibition, clandestine violations and re-interpretations persist, creating a gap between declared policy and actual practice, particularly after 9/11 (reflected in the peak around 2001). `Accessibility_collapse` is high (0.95) because the legal framework aims to make torture legally and morally inaccessible as an option. `Resistance` is high (0.75) due to persistent arguments from state security actors for 'flexibility' or 'necessity' in extreme circumstances.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of detainees and human rights advocates, this constraint is a vital 'rope' or even a 'mountain' of moral principle, providing essential protection. However, from the perspective of state security agencies and interrogators, it operates as a 'snare' or 'tangled_rope', imposing severe restrictions and perceived operational costs, which they often resist or attempt to circumvent. The engine's per-seat classification will capture this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Detainees and human rights advocates are clear beneficiaries, as the constraint protects fundamental rights. The international legal system acts as the agenda-setter, defining and upholding the standard. State security agencies and individual interrogators are the primary payers, as the constraint directly restricts their operational methods and imposes legal obligations. States claiming security exceptions are also payers, as they face international legal and reputational costs for non-compliance, even as they attempt to reinterpret or circumvent the prohibition.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (prohibiting torture) remains acutely live, as evidenced by ongoing violations and the persistent need for its enforcement. There is no evidence of mandatrophy; rather, the challenge is one of continuous contestation and enforcement against powerful state interests. The high extractiveness and suppression are not due to an outdated mandate, but to the inherent conflict between the absolute prohibition and state actors who perceive a 'necessity' for prohibited practices.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    compliance_vs_circumvention,
    'To what extent does the observed ''theater_ratio'' reflect genuine, albeit imperfect, compliance versus deliberate, clandestine circumvention or re-interpretation of the prohibition?',
    'Independent, unhindered access for human rights monitors to detention facilities, whistleblower protections for security personnel, and robust judicial review of interrogation practices.',
    'If primarily circumvention, the effective suppression and extractiveness are higher than measured, as the constraint is actively subverted. If primarily imperfect compliance, the constraint''s ideal function as a ''rope'' is more attainable with better enforcement mechanisms.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(compliance_vs_circumvention, empirical, 'Distinguishing between performative compliance and active subversion of the torture prohibition.').

omega_variable(
    structural_vs_internalized_resistance,
    'Is the resistance to the absolute prohibition primarily structural (e.g., lack of alternative intelligence-gathering methods) or internalized (e.g., a belief among security actors that torture is a ''necessary evil'' or effective)?',
    'Empirical studies on the effectiveness of non-coercive interrogation techniques, and sociological analysis of security culture and professional identity within state agencies.',
    'If resistance is primarily structural, the constraint''s extractiveness is a genuine cost of operational change. If internalized, the constraint faces a deeper challenge of normative shift, and its effective suppression is higher due to self-justifying rationales.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(structural_vs_internalized_resistance, empirical, 'Understanding the root causes of state resistance to the absolute prohibition.').

omega_variable(
    kernel_reading_adherence,
    'Is the ''absolute_prohibition'' reading consistently applied by all relevant international and national judicial bodies, or do some implicitly or explicitly lean towards ''contextual_necessity'' or ''proportionality_balancing'' in their rulings?',
    'Systematic review of jurisprudence from international criminal courts, regional human rights courts, and national supreme courts concerning Common Article 3 and related prohibitions.',
    'If judicial bodies consistently uphold the absolute prohibition, it strengthens the constraint''s ''rope'' classification. If they frequently introduce exceptions or balancing tests, it indicates a drift towards sibling readings, weakening the absolute nature of this constraint and increasing its ''tangled_rope'' or ''snare'' characteristics for detainees.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_adherence, conceptual, 'Consistency of judicial interpretation of the absolute prohibition.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(humane_treatment_standard__absolute_prohibition, 1949, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(huma_tr_t1949, humane_treatment_standard__absolute_prohibition, theater_ratio, 1949, 0.2).
narrative_ontology:measurement(huma_tr_t1969, humane_treatment_standard__absolute_prohibition, theater_ratio, 1969, 0.25).
narrative_ontology:measurement(huma_tr_t1989, humane_treatment_standard__absolute_prohibition, theater_ratio, 1989, 0.35).
narrative_ontology:measurement(huma_tr_t2001, humane_treatment_standard__absolute_prohibition, theater_ratio, 2001, 0.55).
narrative_ontology:measurement(huma_tr_t2012, humane_treatment_standard__absolute_prohibition, theater_ratio, 2012, 0.5).
narrative_ontology:measurement(huma_tr_t2024, humane_treatment_standard__absolute_prohibition, theater_ratio, 2024, 0.45).

% Extraction over time
narrative_ontology:measurement(huma_be_t1949, humane_treatment_standard__absolute_prohibition, base_extractiveness, 1949, 0.7).
narrative_ontology:measurement(huma_be_t1969, humane_treatment_standard__absolute_prohibition, base_extractiveness, 1969, 0.75).
narrative_ontology:measurement(huma_be_t1989, humane_treatment_standard__absolute_prohibition, base_extractiveness, 1989, 0.8).
narrative_ontology:measurement(huma_be_t2001, humane_treatment_standard__absolute_prohibition, base_extractiveness, 2001, 0.9).
narrative_ontology:measurement(huma_be_t2012, humane_treatment_standard__absolute_prohibition, base_extractiveness, 2012, 0.88).
narrative_ontology:measurement(huma_be_t2024, humane_treatment_standard__absolute_prohibition, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(huma_su_t1949, humane_treatment_standard__absolute_prohibition, suppression_requirement, 1949, 0.75).
narrative_ontology:measurement(huma_su_t1969, humane_treatment_standard__absolute_prohibition, suppression_requirement, 1969, 0.8).
narrative_ontology:measurement(huma_su_t1989, humane_treatment_standard__absolute_prohibition, suppression_requirement, 1989, 0.85).
narrative_ontology:measurement(huma_su_t2001, humane_treatment_standard__absolute_prohibition, suppression_requirement, 2001, 0.95).
narrative_ontology:measurement(huma_su_t2012, humane_treatment_standard__absolute_prohibition, suppression_requirement, 2012, 0.92).
narrative_ontology:measurement(huma_su_t2024, humane_treatment_standard__absolute_prohibition, suppression_requirement, 2024, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(humane_treatment_standard__absolute_prohibition, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
