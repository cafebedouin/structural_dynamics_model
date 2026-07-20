% ============================================================================
% CONSTRAINT STORY: common_article_3_scope__state_centric_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_common_article_3_scope__state_centric_reading, []).

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
 *   constraint_id: common_article_3_scope__state_centric_reading
 *   human_readable: State-Centric Threshold Reading of Common Article 3 Scope
 *   domain: legal/international_humanitarian_law
 *
 * SUMMARY:
 *   This constraint instantiates the state-centric reading of the
 *   common_article_3_scope kernel. Common Article 3 of the 1949 Geneva
 *   Conventions sets minimum standards for non-international armed conflicts.
 *   The state-centric reading limits CA3 application to conflicts meeting
 *   intensity and organization thresholds, excluding low-level violence and
 *   law enforcement. This reading is contested by an expansive human rights
 *   reading (applying CA3 to any organized armed violence) and an ICRC
 *   customary reading (tying scope to evolving state practice). The
 *   state-centric reading structurally benefits sovereign states and state
 *   security forces while irregular combatants and civilians in internal
 *   disturbances bear the costs of exclusion.
 *
 * KEY AGENTS:
 *   - sovereign_states: Primary agenda-setter and beneficiary (institutional/arbitrage) â crafts threshold doctrine and retains operational discretion.
 *   - state_security_forces: Secondary beneficiary (organized/constrained) â conducts operations under the shield of threshold exclusions.
 *   - irregular_combatants: Primary target (powerless/trapped) â denied protections based on organizational and intensity criteria they cannot control.
 *   - civilians_in_internal_disturbances: Secondary target (powerless/trapped) â excluded from CA3 in sub-threshold violence.
 *   - icrc: Analytical observer (institutional/analytical) â advocates broader application but is structurally resisted.
 *   - human_rights_tribunals: Analytical observer (institutional/analytical) â applies parallel human rights law but cannot bridge the protection gap.
 *   - local_humanitarian_actors: Excluded voice (moderate/constrained) â witnesses threshold-crossing conditions but lacks standing to challenge classifications.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(common_article_3_scope__state_centric_reading, 0.72).
domain_priors:suppression_score(common_article_3_scope__state_centric_reading, 0.85).
domain_priors:theater_ratio(common_article_3_scope__state_centric_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(common_article_3_scope__state_centric_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(common_article_3_scope__state_centric_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(common_article_3_scope__state_centric_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(common_article_3_scope__state_centric_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(common_article_3_scope__state_centric_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(common_article_3_scope__state_centric_reading, tangled_rope).
narrative_ontology:human_readable(common_article_3_scope__state_centric_reading, "State-Centric Threshold Reading of Common Article 3 Scope").
narrative_ontology:topic_domain(common_article_3_scope__state_centric_reading, "legal/international_humanitarian_law").

domain_priors:requires_active_enforcement(common_article_3_scope__state_centric_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(common_article_3_scope__state_centric_reading, '21afd47f-ae23-4d98-a8d6-958dbd860c10').
narrative_ontology:cs_kernel_codification('21afd47f-ae23-4d98-a8d6-958dbd860c10', formalized).
narrative_ontology:cs_authority_grounding('21afd47f-ae23-4d98-a8d6-958dbd860c10', lineage).
narrative_ontology:cs_interpretation_layer_present('21afd47f-ae23-4d98-a8d6-958dbd860c10').
narrative_ontology:cs_reading_relation('21afd47f-ae23-4d98-a8d6-958dbd860c10', common_article_3_scope__expansive_human_rights_reading, coexists_with).
narrative_ontology:cs_reading_relation('21afd47f-ae23-4d98-a8d6-958dbd860c10', common_article_3_scope__icrc_customary_reading, influences).
narrative_ontology:cs_axiom('21afd47f-ae23-4d98-a8d6-958dbd860c10', foundational, state_sovereignty_as_threshold_gate).
narrative_ontology:cs_axiom_status(state_sovereignty_as_threshold_gate, holdable).
narrative_ontology:cs_axiom_grounding('21afd47f-ae23-4d98-a8d6-958dbd860c10', state_sovereignty_as_threshold_gate, conventional).
narrative_ontology:cs_axiom('21afd47f-ae23-4d98-a8d6-958dbd860c10', foundational, organization_intensity_as_objective_criteria).
narrative_ontology:cs_axiom_status(organization_intensity_as_objective_criteria, holdable).
narrative_ontology:cs_axiom_grounding('21afd47f-ae23-4d98-a8d6-958dbd860c10', organization_intensity_as_objective_criteria, empirically_contingent).
narrative_ontology:cs_reference_frame('21afd47f-ae23-4d98-a8d6-958dbd860c10', state_sovereignty_threshold_framework).
narrative_ontology:cs_drift_state('21afd47f-ae23-4d98-a8d6-958dbd860c10', contemporary_counterterrorism_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('21afd47f-ae23-4d98-a8d6-958dbd860c10', '').
narrative_ontology:cs_kernel_id(common_article_3_scope__state_centric_reading, common_article_3_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(common_article_3_scope__state_centric_reading, sovereign_states).
narrative_ontology:constraint_beneficiary(common_article_3_scope__state_centric_reading, state_security_forces).
narrative_ontology:constraint_victim(common_article_3_scope__state_centric_reading, irregular_combatants).
narrative_ontology:constraint_victim(common_article_3_scope__state_centric_reading, civilians_in_internal_disturbances).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Determine through national legislation, military manuals, and international litigation whether a situation meets the intensity and organization thresholds for CA3 application. Retain maximum discretion to classify internal violence as law enforcement or internal disturbance rather than armed conflict, thereby avoiding international humanitarian law obligations.
narrative_ontology:constraint_stakeholder(common_article_3_scope__state_centric_reading, sovereign_states, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(common_article_3_scope__state_centric_reading, sovereign_states, beneficiary).

% Conduct operations against irregular groups and in internal disturbances. Operations falling below the intensity and organization thresholds are governed by domestic law and human rights frameworks rather than the more restrictive targeting and detention rules of CA3.
narrative_ontology:constraint_stakeholder(common_article_3_scope__state_centric_reading, state_security_forces, beneficiary,
    organized, biographical, constrained, national).

% Engage in armed activity against state forces but lack the organizational structure or operate in conflicts below the intensity threshold. Are denied CA3 protections including humane treatment, fair trial guarantees, and the prohibition of torture, and can be prosecuted under domestic law without the combatant privilege. Have no legal avenue to compel classification as a party to an armed conflict.
narrative_ontology:constraint_stakeholder(common_article_3_scope__state_centric_reading, irregular_combatants, payer,
    powerless, immediate, trapped, local).

% Live in areas affected by riots, low-level violence, or counter-terrorism operations that states classify below the CA3 threshold. Are excluded from the substantive protections of CA3 such as collection of the wounded and humane treatment when the state frames the situation as law enforcement or internal disturbance.
narrative_ontology:constraint_stakeholder(common_article_3_scope__state_centric_reading, civilians_in_internal_disturbances, payer,
    powerless, immediate, trapped, local).

% Advocates for the broadest possible application of CA3 and challenges state threshold determinations. Publishes commentaries and engages bilaterally with states, but its legal position is systematically resisted by the state-centric reading's threshold requirements.
narrative_ontology:constraint_stakeholder(common_article_3_scope__state_centric_reading, icrc, observer,
    institutional, civilizational, analytical, global).

% Apply international human rights law to situations that fall below the CA3 threshold. Their jurisprudence creates parallel protective obligations, but they do not adjudicate CA3 applicability directly, leaving a protection gap where states deny both IHL and effective human rights remedies.
narrative_ontology:constraint_stakeholder(common_article_3_scope__state_centric_reading, human_rights_tribunals, observer,
    institutional, generational, analytical, continental).

% Local aid workers and medical personnel operating in sub-threshold conflict zones. They witness conditions that meet intensity criteria but lack legal standing to challenge state threshold determinations before international tribunals, and their access depends on state consent.
narrative_ontology:constraint_stakeholder(common_article_3_scope__state_centric_reading, local_humanitarian_actors, excluded,
    moderate, immediate, constrained, local).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(common_article_3_scope__state_centric_reading, sovereign_states).
narrative_ontology:fixing_cost_class(common_article_3_scope__state_centric_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the expectations of states regarding when international humanitarian law displaces domestic law enforcement authority, preserving a sphere of sovereign discretion over internal disturbances and low-level violence while establishing a minimum humanitarian floor for clear armed conflicts.
% TRANSFER_FUNCTION: Transfers legal protection and procedural guarantees from irregular combatants and civilians in sub-threshold violence to sovereign states, which gain operational discretion and freedom from IHL compliance and oversight.
% ABSENT_VOICES: Local populations and affected communities in sub-threshold conflicts lack standing before international tribunals to challenge the threshold determination itself; their objections to the narrow classification are mediated through state-controlled domestic legal processes.
% DISAPPEARANCE_RATIONALE: If the state-centric threshold reading vanished and CA3 applied to all organized armed violence regardless of intensity, states would lose the legal discretion to classify counter-insurgency and counter-terrorism operations as law enforcement. Military manuals, rules of engagement, and detention policies would require rewriting; domestic criminal prosecutions of irregular fighters would decrease relative to Geneva-compliant treatment; and international oversight mechanisms would expand dramatically.
% FOUNDING_PROBLEM: The 1949 Geneva Conventions needed to define when a non-international armed conflict exists so that states would accept a minimum humanitarian floor without surrendering sovereignty over ordinary riots, isolated violence, and internal disturbances.
% FOUNDING_PROBLEM_CORROBORATION: States and the ICRC attest the problem remains live. However, human rights tribunals and academic critics attest that the threshold doctrine now systematically underprotects civilians in protracted low-intensity violence that functionally resembles armed conflict, suggesting the arrangement persists beyond its founding problem.
narrative_ontology:disappearance_verdict(common_article_3_scope__state_centric_reading, world_rearranges).
narrative_ontology:founding_problem_status(common_article_3_scope__state_centric_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(common_article_3_scope__state_centric_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(common_article_3_scope__state_centric_reading, 'none', 1).
narrative_ontology:epsilon_provenance(common_article_3_scope__state_centric_reading, 0.72, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(common_article_3_scope__state_centric_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(common_article_3_scope__state_centric_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(common_article_3_scope__state_centric_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is high (0.72) because the threshold doctrine systematically withholds CA3 protections from irregular combatants and civilians in protracted low-intensity violence. Suppression is very high (0.85) because the reading is actively enforced through state legal classification, military manuals, tribunal deference to state determinations, and the absence of accessible remedies for excluded parties. Theater ratio is moderate (0.45): the intensity and organization criteria are presented as objective legal tests, but in practice they are frequently deployed as performative sovereignty shields, particularly in counterterrorism contexts. Accessibility collapse is high (0.80) because once a state classifies violence below threshold, legal alternatives for obtaining CA3 protections effectively vanish for the affected populations. Resistance is moderate (0.55): the ICRC, human rights bodies, and some judicial opinions resist the narrow reading, but states remain the dominant interpretive authority.
 *
 * PERSPECTIVAL GAP:
 *   The sovereign state seat experiences the constraint as a necessary legal technology preserving domestic jurisdiction and military flexibility in unstable environments. The irregular combatant and civilian seats experience it as an arbitrary barrier that strips them of protections based on a classification they cannot control. The ICRC and tribunal seats see a contested interpretive space where humanitarian purpose is narrowed by state practice, but they lack the structural leverage to alter the threshold doctrine.
 *
 * DIRECTIONALITY LOGIC:
 *   Sovereign_states and state_security_forces are structural beneficiaries (low d): the constraint subsidizes their operational discretion by narrowing the window of IHL applicability. Irregular_combatants and civilians_in_internal_disturbances are structural targets (high d): the constraint extracts legal protections from them and offers no exit from the threshold determination. Local_humanitarian_actors are excluded from the conversation entirely, receiving neither coordination benefit nor direct extraction but bearing witness to its effects.
 *
 * MANDATROPHY ANALYSIS:
 *   The state-centric reading risks mandatrophy if the original purpose was to ensure minimum humanity in civil wars, but the current function is to shield states from scrutiny in protracted low-intensity campaigns. The temporal measurement series shows rising extraction and suppression from 1949 to the present, with a sharp peak during the post-2001 counterterrorism era, suggesting the threshold has shifted from a genuine coordination device toward an extractive shield. However, because the coordination function among states (legal certainty, sovereignty preservation) remains partially real and actively defended, the classification stays tangled_rope rather than degrading fully into snare or piton.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ca3_scope_kernel_reading_contest,
    'This constraint instantiates the state-centric reading of the common_article_3_scope kernel. The expansive_human_rights_reading would extend CA3 to any organized armed violence regardless of threshold, and the icrc_customary_reading would tie scope to evolving practice. Where is the disagreement located: in the empirical assessment of conflict characteristics, or in the normative priority of sovereignty versus humanitarian protection?',
    'Comparative state practice review and tribunal jurisprudence mapping to identify whether threshold disputes turn on factual findings (intensity/organization) or normative commitments (sovereignty vs. humanity principle).',
    'If located in empirical assessment, the state-centric reading is a contingent legal test; if located in normative priority, it is a structural commitment system with distinct axioms.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ca3_scope_kernel_reading_contest, conceptual, 'Locator for disagreement between state-centric and sibling readings of CA3 scope.').

omega_variable(
    threshold_doctrine_as_extraction,
    'Is the intensity and organization threshold doctrine a necessary legal technology for distinguishing armed conflict from internal disturbances, or does it function as a suppression mechanism that systematically withholds CA3 protections from irregular violence?',
    'Quantitative correlation between threshold findings and state regime type or strategic interest, controlling for empirical conflict intensity measured against independent conflict datasets.',
    'If correlation is high, the threshold is extractive cover; if low, it is a genuine coordination device preserving legal certainty.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(threshold_doctrine_as_extraction, empirical, 'Whether CA3 threshold doctrine is coordination or extraction.').

omega_variable(
    enforcement_hardening_vs_normalization,
    'Does the state-centric reading persist because states actively defend it with escalating legal arguments and classification practices, or because alternative readings lack institutional traction in state-centric forums?',
    'Tracking litigation patterns and state manual revisions: if challenges to threshold determinations increase but success rates decrease, this indicates hardening; if challenge volume decreases, this indicates normalization or decay of humanitarian oversight.',
    'Hardening would suggest the constraint is becoming more extractive over time; decay would suggest it is drifting toward piton status where it persists by inertia rather than active defense.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_hardening_vs_normalization, empirical, 'Whether persistence is active defense or institutional inertia.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(common_article_3_scope__state_centric_reading, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comm_tr_t0, common_article_3_scope__state_centric_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(comm_tr_t15, common_article_3_scope__state_centric_reading, theater_ratio, 15, 0.25).
narrative_ontology:measurement(comm_tr_t30, common_article_3_scope__state_centric_reading, theater_ratio, 30, 0.3).
narrative_ontology:measurement(comm_tr_t45, common_article_3_scope__state_centric_reading, theater_ratio, 45, 0.35).
narrative_ontology:measurement(comm_tr_t60, common_article_3_scope__state_centric_reading, theater_ratio, 60, 0.55).
narrative_ontology:measurement(comm_tr_t75, common_article_3_scope__state_centric_reading, theater_ratio, 75, 0.45).

% Extraction over time
narrative_ontology:measurement(comm_be_t0, common_article_3_scope__state_centric_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(comm_be_t15, common_article_3_scope__state_centric_reading, base_extractiveness, 15, 0.45).
narrative_ontology:measurement(comm_be_t30, common_article_3_scope__state_centric_reading, base_extractiveness, 30, 0.5).
narrative_ontology:measurement(comm_be_t45, common_article_3_scope__state_centric_reading, base_extractiveness, 45, 0.6).
narrative_ontology:measurement(comm_be_t60, common_article_3_scope__state_centric_reading, base_extractiveness, 60, 0.75).
narrative_ontology:measurement(comm_be_t75, common_article_3_scope__state_centric_reading, base_extractiveness, 75, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(comm_su_t0, common_article_3_scope__state_centric_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(comm_su_t15, common_article_3_scope__state_centric_reading, suppression_requirement, 15, 0.5).
narrative_ontology:measurement(comm_su_t30, common_article_3_scope__state_centric_reading, suppression_requirement, 30, 0.55).
narrative_ontology:measurement(comm_su_t45, common_article_3_scope__state_centric_reading, suppression_requirement, 45, 0.65).
narrative_ontology:measurement(comm_su_t60, common_article_3_scope__state_centric_reading, suppression_requirement, 60, 0.9).
narrative_ontology:measurement(comm_su_t75, common_article_3_scope__state_centric_reading, suppression_requirement, 75, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(common_article_3_scope__state_centric_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(common_article_3_scope__state_centric_reading, common_article_3_scope__expansive_human_rights_reading).
narrative_ontology:affects_constraint(common_article_3_scope__state_centric_reading, common_article_3_scope__icrc_customary_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the common_article_3_scope kernel. The kernel decomposes into structurally distinct constraints because the state-centric, expansive human rights, and ICRC customary readings produce different epsilon values, beneficiary/victim structures, and enforcement patterns.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
