% ============================================================================
% CONSTRAINT STORY: humane_treatment_standard__proportionality_balancing
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_humane_treatment_standard__proportionality_balancing, []).

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
 *   constraint_id: humane_treatment_standard__proportionality_balancing
 *   human_readable: Common Article 3 Proportionality Balancing: Detainee Dignity vs. Security Needs
 *   domain: international/humanitarian/legal
 *
 * SUMMARY:
 *   Common Article 3 of the Geneva Conventions establishes baseline
 *   protections for detained persons: prohibition on torture, cruel
 *   treatment, and degrading punishment. The proportionality balancing
 *   reading interprets this requirement as permitting enhanced interrogation
 *   techniques when deemed proportional to security threats and subject to
 *   judicial review and medical oversight. This is ONE reading of a contested
 *   kernel (humane_treatment_standard). The absolute_prohibition reading
 *   reads the same text as establishing non-derogable minimum standards with
 *   no permitted exceptions. The contextual_necessity reading reads it as
 *   establishing a baseline that can be overridden when national security
 *   imperatives demand it. This story instantiates the
 *   proportionality_balancing reading: the constraint is that interrogators
 *   must justify techniques as proportional, courts review the
 *   proportionality claim, and detainees' dignity is protected by
 *   case-by-case gatekeeping rather than absolute prohibition. The reading
 *   operationalizes 'humane treatment' as a balancing problem, not a
 *   threshold problem, which structurally enables greater interrogation
 *   authority than absolute prohibition while nominally maintaining
 *   humanitarian constraints.
 *
 * KEY AGENTS:
 *   - State security apparatus: institutional beneficiary; sets interrogation standards; administers proportionality assessments in classified contexts; benefits from discretion
 *   - Detained persons: powerless payers; subject to techniques authorized under proportionality standard; trapped without appeal mechanisms outside state apparatus
 *   - Independent judiciary: institutional beneficiary and gatekeeper; positioned as proportionality reviewer; constrained by security classification and executive deference
 *   - Medical oversight bodies: organized beneficiaries; participate in fitness and harm assessments; constrained by employment relationship to detaining state
 *   - Absolute prohibition advocates: organized but excluded; hold contradictory reading; maintain advocacy but lack structural power within proportionality framework
 *   - International monitoring bodies: organized observers; report on compliance but lack enforcement power
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(humane_treatment_standard__proportionality_balancing, 0.62).
domain_priors:suppression_score(humane_treatment_standard__proportionality_balancing, 0.58).
domain_priors:theater_ratio(humane_treatment_standard__proportionality_balancing, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(humane_treatment_standard__proportionality_balancing, extractiveness, 0.62).
narrative_ontology:constraint_metric(humane_treatment_standard__proportionality_balancing, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(humane_treatment_standard__proportionality_balancing, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(humane_treatment_standard__proportionality_balancing, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(humane_treatment_standard__proportionality_balancing, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(humane_treatment_standard__proportionality_balancing, tangled_rope).
narrative_ontology:human_readable(humane_treatment_standard__proportionality_balancing, "Common Article 3 Proportionality Balancing: Detainee Dignity vs. Security Needs").
narrative_ontology:topic_domain(humane_treatment_standard__proportionality_balancing, "international/humanitarian/legal").

domain_priors:requires_active_enforcement(humane_treatment_standard__proportionality_balancing).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(humane_treatment_standard__proportionality_balancing, '1891a201-68cd-42fa-b904-1d9e0fb91435').
narrative_ontology:cs_kernel_codification('1891a201-68cd-42fa-b904-1d9e0fb91435', fixed_text).
narrative_ontology:cs_authority_grounding('1891a201-68cd-42fa-b904-1d9e0fb91435', lineage).
narrative_ontology:cs_interpretation_layer_present('1891a201-68cd-42fa-b904-1d9e0fb91435').
narrative_ontology:cs_reading_relation('1891a201-68cd-42fa-b904-1d9e0fb91435', humane_treatment_standard__absolute_prohibition, coexists_with).
narrative_ontology:cs_reading_relation('1891a201-68cd-42fa-b904-1d9e0fb91435', humane_treatment_standard__contextual_necessity, influences).
narrative_ontology:cs_axiom('1891a201-68cd-42fa-b904-1d9e0fb91435', foundational, proportionality_balancing_legitimate).
narrative_ontology:cs_axiom_status(proportionality_balancing_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('1891a201-68cd-42fa-b904-1d9e0fb91435', proportionality_balancing_legitimate, deontological).
narrative_ontology:cs_axiom('1891a201-68cd-42fa-b904-1d9e0fb91435', foundational, case_by_case_gatekeeping_constrains).
narrative_ontology:cs_axiom_status(case_by_case_gatekeeping_constrains, holdable).
narrative_ontology:cs_axiom_grounding('1891a201-68cd-42fa-b904-1d9e0fb91435', case_by_case_gatekeeping_constrains, empirically_contingent).
narrative_ontology:cs_reference_frame('1891a201-68cd-42fa-b904-1d9e0fb91435', common_article_three_balanced_humanitarian_standard).
narrative_ontology:cs_drift_state('1891a201-68cd-42fa-b904-1d9e0fb91435', post_enhanced_interrogation_normalization, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('1891a201-68cd-42fa-b904-1d9e0fb91435', '').
narrative_ontology:cs_kernel_id(humane_treatment_standard__proportionality_balancing, humane_treatment_standard).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(humane_treatment_standard__proportionality_balancing, state_security_apparatus).
narrative_ontology:constraint_victim(humane_treatment_standard__proportionality_balancing, detained_persons).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(humane_treatment_standard__proportionality_balancing, independent_judiciary).
narrative_ontology:constraint_beneficiary(humane_treatment_standard__proportionality_balancing, medical_oversight_bodies).
narrative_ontology:constraint_beneficiary(humane_treatment_standard__proportionality_balancing, interrogators_operational_level).
narrative_ontology:constraint_victim(humane_treatment_standard__proportionality_balancing, interrogators_operational_level).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operates detention and interrogation systems under the proportionality reading. Sets interrogation techniques deemed 'proportional' within this framework—sleep deprivation, stress positions, sensory manipulation—justified as extracting actionable intelligence proportional to security threat level. Administers procedural safeguards (medical oversight, time limits) that performatively bind the constraint while retaining substantial operational discretion over what counts as 'proportional' in classified contexts. Benefits from the reading's case-by-case gatekeeping because each determination can be tailored to specific threat assessments.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__proportionality_balancing, state_security_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).

% Subject to interrogation under proportionality standards. Their 'protected' status (not absolute prohibition, but dignified treatment) is conditional on the state's assessment of their threat level and the security value of information sought. They have no say in whether interrogation techniques are proportional; appeal mechanisms are internal to the state apparatus and operate under classification constraints. Physical and psychological harm flow from techniques authorized under the proportionality standard.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__proportionality_balancing, detained_persons, payer,
    powerless, immediate, trapped, local).

% Positioned as the procedural gatekeeper in the proportionality reading—the institution that reviews state security determinations for proportionality compliance. Benefits from gatekeeping authority and institutional legitimacy that the reading confers. Constrained exit because removing judicial review would collapse the proportionality standard entirely; the reading's plausibility depends on courts' presence. In practice, security classification and executive-branch deference systematically narrow judicial review capacity.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__proportionality_balancing, independent_judiciary, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(humane_treatment_standard__proportionality_balancing, independent_judiciary, agenda_setter).

% Participate in proportionality assessments by certifying detainee fitness for interrogation and monitoring for serious harm thresholds. Derive legitimacy and resources from the role. Constrained by classification restrictions, by state pressure to certify as 'fit,' and by conflict-of-interest (they work for the detaining state). The proportionality reading positions them as safeguards; their actual capacity to refuse certification or exit the role is limited.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__proportionality_balancing, medical_oversight_bodies, beneficiary,
    organized, biographical, constrained, national).

% International humanitarian law bodies (Red Cross, treaty bodies, UN mechanisms) observe and assess state compliance with Common Article 3. They have reporting authority but no enforcement power over proportionality determinations made by states. Their observations feed advocacy and norm-setting but do not directly alter the constraint's operation in any single state. Can exit the monitoring relationship by withdrawing access; states can refuse cooperation.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__proportionality_balancing, international_monitoring_bodies, observer,
    organized, generational, mobile, global).

% Hold that Common Article 3 mandates absolute prohibition on torture and degrading treatment without exception. They are systematically excluded from the proportionality reading's gatekeeping institutions and procedural frameworks because the reading's core premise—that proportionality balancing is legitimate—directly contradicts their advocacy premise. They remain present in litigation and legislative processes but lack structural power to enforce the absolute reading within states using the proportionality framework.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__proportionality_balancing, absolute_prohibition_advocates, excluded,
    organized, generational, constrained, global).

% Operate under proportionality standards that authorize specific techniques within case-by-case bounds. Benefit from the framework's discretion (clearer operational authorization than absolute prohibition) and performative legitimacy (courts and medical oversight provide institutional cover). Bear compliance costs (procedural documentation, medical consultation) and legal/reputational risk if techniques later deemed disproportionate. Constrained by institutional hierarchy and by classification that prevents public knowledge of actual practices.
narrative_ontology:constraint_stakeholder(humane_treatment_standard__proportionality_balancing, interrogators_operational_level, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(humane_treatment_standard__proportionality_balancing, interrogators_operational_level, payer).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(humane_treatment_standard__proportionality_balancing, state_security_apparatus).
narrative_ontology:fixing_cost_class(humane_treatment_standard__proportionality_balancing, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates state security imperatives with international humanitarian commitments by establishing a procedural framework (judicial review, medical oversight) that permits interrogation while nominally protecting detainee dignity. Solves the institutional problem of how states can conduct counterterrorism operations without appearing to violate absolute humanitarian law norms.
% TRANSFER_FUNCTION: Transfers interrogation authority from humanitarian legal absolutes to courts and state security apparatus. Detainees' rights shift from unconditional (absolute prohibition) to conditional (proportionality assessment). Information extracted through interrogation under proportionality standards flows to state security apparatus; compliance costs (procedural burdens, oversight) flow to interrogators and medical bodies; legitimacy flows to independent judiciary as gatekeepers.
% ABSENT_VOICES: Absolute prohibition advocates and detained persons' independent legal representatives are structurally excluded from proportionality determinations. Detainees' own assessment of whether techniques are proportional to their situation is not solicited. Voices that would argue enhanced interrogation does not improve security outcomes (empirical challenge to the proportionality calculus itself) are marginalized in security-classified contexts.
% DISAPPEARANCE_RATIONALE: If the proportionality balancing standard and its procedural apparatus vanished, states would either adopt absolute prohibition (reordering interrogation practice entirely) or shift to contextual necessity framing (no gatekeeping, unlimited discretion). The interrogation ecosystem, classification practices, and medical-security partnerships would reorganize around whichever reading replaced it.
% FOUNDING_PROBLEM: Post-2001 counterterrorism required capturing and interrogating detainees; absolute prohibition on interrogation techniques collided with state security exigencies; absolute prohibition on torture was irreconcilable with enhanced interrogation practices. The proportionality reading was adopted as a middle path: humanitarian protections remain nominally in force but subject to case-by-case security assessment.
% FOUNDING_PROBLEM_CORROBORATION: State security apparatus and judicial institutions attests the founding problem remains live: terrorist threats require interrogation and proportionality enables both security and humanitarian compliance. Absolute prohibition advocates and international humanitarian monitors attest the founding problem is a false dilemma manufactured by states unwilling to accept interrogation limits; they argue enhanced interrogation does not measurably improve security and the 'problem' is actually states' desire to avoid accountability. Empirical research on interrogation efficacy is cited by both sides; no external authority has definitively resolved which reading correctly describes the founding situation.
narrative_ontology:disappearance_verdict(humane_treatment_standard__proportionality_balancing, world_rearranges).
narrative_ontology:founding_problem_status(humane_treatment_standard__proportionality_balancing, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(humane_treatment_standard__proportionality_balancing, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(humane_treatment_standard__proportionality_balancing, 'none', 1).
narrative_ontology:epsilon_provenance(humane_treatment_standard__proportionality_balancing, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(humane_treatment_standard__proportionality_balancing_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(humane_treatment_standard__proportionality_balancing, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(humane_treatment_standard__proportionality_balancing_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The proportionality reading produces moderate-to-high extractiveness (0.62 at interval end) because the balancing framework legitimizes enhanced interrogation while purporting to protect detainee dignity. Suppression is slightly lower than extractiveness (0.58) because courts, medical bodies, and classification provide multiple points where dissent could theoretically surface, even though state structural advantage limits that dissent's effect. Theater ratio rises from 0.28 to 0.41 over the interval, reflecting the increasing performative character of procedural review: as states develop refined proportionality jurisprudence and medical protocols, the procedural apparatus becomes more elaborate while actual detainee protection may not proportionally increase. The measurement series tracks the constraint's operation over an interval where proportionality jurisprudence was codified and normalized. Accessibility collapse is moderate (0.48) because absolute prohibition remains a live competing reading and detainees can theoretically appeal to international bodies, even if practically those mechanisms rarely reverse proportionality determinations. Resistance is high (0.72) because absolute prohibition advocates, human rights organizations, and some judiciary continue active resistance to the proportionality reading's permissiveness.
 *
 * PERSPECTIVAL GAP:
 *   From the state security apparatus seat, the proportionality reading is genuine coordination between humanitarian law and security necessity; interrogation techniques are legitimate where proportionate and subject to oversight. From the detained person seat, the same structure appears as conditional authorization of techniques that cause physical and psychological harm, with gatekeeping institutions controlled by or aligned with the state. From the absolute prohibition advocate seat, the reading is fundamentally incoherent: either torture is prohibited absolutely, or it is permitted; 'proportional torture' is a conceptual impossibility. The engine computes these divergent type classifications (snare from the detainee seat, rope from the state seat, mountain from the absolute prohibition seat in its own reading-frame) from the structural data; the proportionality reading itself does not adjudicate which is correct.
 *
 * DIRECTIONALITY LOGIC:
 *   State security apparatus: beneficiary role, institutional power, arbitrage exit (can switch to contextual necessity or absolute prohibition readings if proportionality becomes untenable), derives d near 0.2 (low extraction). Detained persons: payer role, powerless, trapped exit (cannot leave detention except via state release), derives d near 0.95 (full target). Judiciary: dual-positioned beneficiary/agenda-setter, institutional power, constrained exit (cannot exit without delegitimizing the reading), derives d near 0.45 (moderate symmetric burden—legitimacy flows to them but they are constrained by security apparatus). No directionality overrides needed; the structural derivation captures the asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   The proportionality reading carries the risk of mandate atrophy: it was adopted post-2001 to solve the coordination problem of reconciling security imperatives with humanitarian law. Over 20+ years of proportionality jurisprudence, the mandate (protecting detainee dignity while enabling legitimate interrogation) has been increasingly subordinated to security imperatives. The theater ratio's rise reflects this: procedures (medical oversight, judicial review) have become more elaborate while detainee protection has not proportionally increased. Mandatrophy is contested because state security apparatus maintains the proportionality framework is functioning as designed (threats remain high, techniques remain proportional to threats), while absolute prohibition advocates argue the mandate is dead and the framework is pure extraction dressed as balancing. International monitors occupy the middle: the mandate is attenuated but not yet wholly moribund.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    proportionality_vs_rationalization,
    'Do case-by-case proportionality determinations genuinely constrain interrogation techniques, or do they rationalize techniques that states have already decided to use?',
    'Retrospective analysis of declassified case files comparing initial security threat assessment to techniques authorized and information actually obtained; comparison of proportionality determinations that were rejected vs. approved; expert review of whether rejections reflect genuine proportionality concern or procedural gatekeeping theater.',
    'If determinations genuinely constrain (rejections occur and deter techniques), the reading is moderately extractive and the constraint operates as balanced. If determinations rationalize post-hoc (rejections are rare and follow pre-selected techniques), the reading is highly extractive and the constraint operates as pure extraction wearing proportionality clothes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proportionality_vs_rationalization, empirical, 'Whether proportionality review constrains or rationalizes interrogation decisions.').

omega_variable(
    reading_logical_status,
    'Is the proportionality balancing reading logically coherent, or does it rest on an internal contradiction between ''proportional torture'' (if torture is torture, it cannot be proportional to any legitimate end) and ''permissible interrogation'' (if interrogation is permissible, the proportionality test is vacuous)?',
    'Philosophical analysis of the proportionality concept as applied to interrogation; examination of whether proportionality determinations actually resolve the core dispute or defer it into classification.',
    'If the reading is incoherent, it is a false balance (snare wearing rope clothing). If coherent, the constraint genuinely partitions the space between absolute and unlimited interrogation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_logical_status, conceptual, 'The logical coherence of proportionality as applied to humane treatment standards.').

omega_variable(
    absolute_prohibition_structural_pressure,
    'Does the proportionality reading reduce pressure from absolute prohibition advocates, or does it sharpen their critique by instantiating a reading they view as philosophically indefensible?',
    'Longitudinal analysis of advocacy intensity and litigation patterns: does proportionality jurisprudence reduce absolute prohibition claims or mobilize them?',
    'If proportionality reduces advocacy pressure, it functions as institutional legitimacy capture. If it sharpens critique, the reading remains under active contest and its authority is fragile.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(absolute_prohibition_structural_pressure, empirical, 'Whether proportionality reading reduces or intensifies pressure from alternative readings.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Do detained persons'' apparent acceptance of proportionality determinations reflect genuine belief in proportionality balancing, or internalized suppression (belief that objection is futile)?',
    'Post-release surveys and retrospective interviews with formerly detained persons about their understanding of interrogation legitimacy; longitudinal analysis of appeal patterns and litigation choices.',
    'If internalized, the measured suppression (0.58) understates the true suppression burden; detainees carry the suppression with them after release. If structural, the measured suppression reflects only external barriers and detainees would immediately shift posture after exit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Structural vs. internalized suppression in proportionality determinations.').

omega_variable(
    kernel_reading_distinction_in_practice,
    'In actual state practice, does the proportionality reading diverge meaningfully from contextual necessity, or do states using proportionality language actually operate under contextual necessity premises (unlimited discretion dressed as balancing)?',
    'Comparative analysis of declassified interrogation authorization documents from states nominally adhering to proportionality vs. states explicitly adopting contextual necessity; examination of whether proportionality rejections occur or whether all proposed techniques are approved.',
    'If meaningfully divergent, the readings are distinct constraints with different structural properties. If convergent, the proportionality reading is a surface change while underlying contextual necessity premises remain operative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_distinction_in_practice, empirical, 'Whether proportionality reading is structurally distinct from contextual necessity in state practice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(humane_treatment_standard__proportionality_balancing, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(huma_tr_t0, humane_treatment_standard__proportionality_balancing, theater_ratio, 0, 0.28).
narrative_ontology:measurement(huma_tr_t5, humane_treatment_standard__proportionality_balancing, theater_ratio, 5, 0.32).
narrative_ontology:measurement(huma_tr_t10, humane_treatment_standard__proportionality_balancing, theater_ratio, 10, 0.37).
narrative_ontology:measurement(huma_tr_t15, humane_treatment_standard__proportionality_balancing, theater_ratio, 15, 0.4).
narrative_ontology:measurement(huma_tr_t20, humane_treatment_standard__proportionality_balancing, theater_ratio, 20, 0.41).
narrative_ontology:measurement(huma_tr_t25, humane_treatment_standard__proportionality_balancing, theater_ratio, 25, 0.41).

% Extraction over time
narrative_ontology:measurement(huma_be_t0, humane_treatment_standard__proportionality_balancing, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(huma_be_t5, humane_treatment_standard__proportionality_balancing, base_extractiveness, 5, 0.54).
narrative_ontology:measurement(huma_be_t10, humane_treatment_standard__proportionality_balancing, base_extractiveness, 10, 0.59).
narrative_ontology:measurement(huma_be_t15, humane_treatment_standard__proportionality_balancing, base_extractiveness, 15, 0.61).
narrative_ontology:measurement(huma_be_t20, humane_treatment_standard__proportionality_balancing, base_extractiveness, 20, 0.62).
narrative_ontology:measurement(huma_be_t25, humane_treatment_standard__proportionality_balancing, base_extractiveness, 25, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(huma_su_t0, humane_treatment_standard__proportionality_balancing, suppression_requirement, 0, 0.52).
narrative_ontology:measurement(huma_su_t5, humane_treatment_standard__proportionality_balancing, suppression_requirement, 5, 0.55).
narrative_ontology:measurement(huma_su_t10, humane_treatment_standard__proportionality_balancing, suppression_requirement, 10, 0.57).
narrative_ontology:measurement(huma_su_t15, humane_treatment_standard__proportionality_balancing, suppression_requirement, 15, 0.58).
narrative_ontology:measurement(huma_su_t20, humane_treatment_standard__proportionality_balancing, suppression_requirement, 20, 0.58).
narrative_ontology:measurement(huma_su_t25, humane_treatment_standard__proportionality_balancing, suppression_requirement, 25, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(humane_treatment_standard__proportionality_balancing, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(humane_treatment_standard__proportionality_balancing, 0.12).
narrative_ontology:affects_constraint(humane_treatment_standard__proportionality_balancing, humane_treatment_standard__absolute_prohibition).
narrative_ontology:affects_constraint(humane_treatment_standard__proportionality_balancing, humane_treatment_standard__contextual_necessity).

% DUAL FORMULATION NOTE:
% The humane_treatment_standard kernel is instantiated by three distinct readings, each authoring a different constraint with different ε values, beneficiary structures, and classification types. The proportionality_balancing reading (this file) interprets Common Article 3 as permitting case-by-case proportional interrogation subject to judicial and medical review. The absolute_prohibition reading interprets the same text as establishing unconditional prohibition. The contextual_necessity reading permits unlimited state discretion when security imperatives are sufficiently grave. Each reading is a separate constraint story with separate .pl files; they are linked via network.affects_constraints to indicate they are readings of the same kernel and structural influence each other (proportionality influences the institutional terrain for both siblings by positioning itself as the moderate option).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
