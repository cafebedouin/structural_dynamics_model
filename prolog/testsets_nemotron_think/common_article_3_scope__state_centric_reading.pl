% ============================================================================
% CONSTRAINT STORY: common_article_3_scope__state_centric_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
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
 *   human_readable: Common Article 3 Threshold Limitation (State-Centric Reading)
 *   domain: legal/humanitarian
 *
 * SUMMARY:
 *   This constraint story captures the state-centric reading of Common
 *   Article 3's scope of application. Under this reading, CA3 applies only
 *   when violence reaches a threshold of intensity (sustained and concerted
 *   military operations) AND the non-state party achieves a threshold of
 *   organization (responsible command, territorial control, sustained
 *   operations). Violence below these thresholds — including counterterrorism
 *   operations, low-level insurgencies, civil unrest, and law enforcement —
 *   remains governed by human rights law and domestic criminal law, not IHL.
 *   This reading maximizes state operational discretion and minimizes
 *   humanitarian obligations in the gray zones where most contemporary state
 *   violence occurs. The claimed type is snare: the coordination story (legal
 *   certainty, preventing IHL overreach) is cover for a structure that
 *   systematically excludes protection for the most vulnerable while
 *   concentrating interpretive power in the hands of the party that benefits
 *   from exclusion.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(common_article_3_scope__state_centric_reading, 0.68).
domain_priors:suppression_score(common_article_3_scope__state_centric_reading, 0.72).
domain_priors:theater_ratio(common_article_3_scope__state_centric_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(common_article_3_scope__state_centric_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(common_article_3_scope__state_centric_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(common_article_3_scope__state_centric_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(common_article_3_scope__state_centric_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(common_article_3_scope__state_centric_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(common_article_3_scope__state_centric_reading, snare).
narrative_ontology:human_readable(common_article_3_scope__state_centric_reading, "Common Article 3 Threshold Limitation (State-Centric Reading)").
narrative_ontology:topic_domain(common_article_3_scope__state_centric_reading, "legal/humanitarian").

domain_priors:requires_active_enforcement(common_article_3_scope__state_centric_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(common_article_3_scope__state_centric_reading, '6caf8d1d-448d-46e6-97f6-530d076c4e3a').
narrative_ontology:cs_kernel_codification('6caf8d1d-448d-46e6-97f6-530d076c4e3a', formalized).
narrative_ontology:cs_authority_grounding('6caf8d1d-448d-46e6-97f6-530d076c4e3a', lineage).
narrative_ontology:cs_interpretation_layer_present('6caf8d1d-448d-46e6-97f6-530d076c4e3a').
narrative_ontology:cs_reading_relation('6caf8d1d-448d-46e6-97f6-530d076c4e3a', common_article_3_scope__expansive_human_rights_reading, coexists_with).
narrative_ontology:cs_reading_relation('6caf8d1d-448d-46e6-97f6-530d076c4e3a', common_article_3_scope__icrc_customary_reading, influences).
narrative_ontology:cs_axiom('6caf8d1d-448d-46e6-97f6-530d076c4e3a', foundational, threshold_based_ihl_application).
narrative_ontology:cs_axiom_status(threshold_based_ihl_application, holdable).
narrative_ontology:cs_axiom_grounding('6caf8d1d-448d-46e6-97f6-530d076c4e3a', threshold_based_ihl_application, conventional).
narrative_ontology:cs_axiom('6caf8d1d-448d-46e6-97f6-530d076c4e3a', foundational, state_discretion_primacy_in_classification).
narrative_ontology:cs_axiom_status(state_discretion_primacy_in_classification, holdable).
narrative_ontology:cs_axiom_grounding('6caf8d1d-448d-46e6-97f6-530d076c4e3a', state_discretion_primacy_in_classification, conventional).
narrative_ontology:cs_axiom('6caf8d1d-448d-46e6-97f6-530d076c4e3a', secondary, law_enforcement_paradigm_supremacy_below_threshold).
narrative_ontology:cs_axiom_status(law_enforcement_paradigm_supremacy_below_threshold, holdable).
narrative_ontology:cs_axiom_grounding('6caf8d1d-448d-46e6-97f6-530d076c4e3a', law_enforcement_paradigm_supremacy_below_threshold, conventional).
narrative_ontology:cs_reference_frame('6caf8d1d-448d-46e6-97f6-530d076c4e3a', post_wwii_ihl_framework).
narrative_ontology:cs_drift_state('6caf8d1d-448d-46e6-97f6-530d076c4e3a', contemporary_counterinsurgency_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('6caf8d1d-448d-46e6-97f6-530d076c4e3a', '').
narrative_ontology:cs_kernel_id(common_article_3_scope__state_centric_reading, common_article_3_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(common_article_3_scope__state_centric_reading, state_governments).
narrative_ontology:constraint_beneficiary(common_article_3_scope__state_centric_reading, national_military_commands).
narrative_ontology:constraint_victim(common_article_3_scope__state_centric_reading, irregular_combatants_below_threshold).
narrative_ontology:constraint_victim(common_article_3_scope__state_centric_reading, civilians_in_low_intensity_conflicts).
narrative_ontology:constraint_victim(common_article_3_scope__state_centric_reading, non_state_armed_groups_excluded).
narrative_ontology:constraint_vindicates(common_article_3_scope__state_centric_reading, state_sovereignty_over_internal_security).
narrative_ontology:constraint_vindicates(common_article_3_scope__state_centric_reading, threshold_based_ihl_application).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interpret and apply CA3 thresholds through domestic legislation, military manuals, and judicial decisions. Retain discretion to classify violence as law enforcement rather than armed conflict, preserving sovereign control over internal security operations. Benefit from narrowed humanitarian obligations in counterinsurgency and counterterrorism contexts.
narrative_ontology:constraint_stakeholder(common_article_3_scope__state_centric_reading, state_governments, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(common_article_3_scope__state_centric_reading, state_governments, beneficiary).

% Operate under rules of engagement shaped by state-centric CA3 interpretation. Gain operational flexibility in low-intensity operations where opponents do not meet organization thresholds. Avoid IHL compliance costs (detention review, humane treatment standards, prohibition on adverse distinction) when adversaries are classified as criminals rather than organized armed groups.
narrative_ontology:constraint_stakeholder(common_article_3_scope__state_centric_reading, national_military_commands, beneficiary,
    organized, biographical, constrained, national).

% Fighters in loosely organized groups, spontaneous uprisings, or decentralized resistance who fall below the organization threshold. Denied combatant privilege and IHL protections (humane treatment, fair trial guarantees, protection from violence to life and person). Subject to domestic criminal law with no equivalent protective framework. Cannot exit the classification — it is imposed by the adversary's legal characterization.
narrative_ontology:constraint_stakeholder(common_article_3_scope__state_centric_reading, irregular_combatants_below_threshold, payer,
    powerless, immediate, trapped, local).

% Populations affected by violence that states classify as law enforcement, riots, or internal disturbances rather than armed conflict. Lose CA3's minimum protections (prohibition on violence to life and person, humiliating treatment, unfair trial, collective punishment). State security operations proceed under human rights law frameworks that permit lethal force under wider circumstances than IHL's conduct-of-hostilities rules. No voice in the classification decision.
narrative_ontology:constraint_stakeholder(common_article_3_scope__state_centric_reading, civilians_in_low_intensity_conflicts, payer,
    powerless, immediate, trapped, local).

% Armed groups with some organization but deemed insufficiently structured under state-centric thresholds (lacking responsible command, territorial control, or sustained operations). Denied ability to invoke IHL protections for their members or to claim belligerent status. Face prosecution for mere participation in hostilities. May seek to increase organization to cross threshold — creating perverse incentive for more hierarchical, controllable structures.
narrative_ontology:constraint_stakeholder(common_article_3_scope__state_centric_reading, non_state_armed_groups_excluded, payer,
    moderate, biographical, constrained, regional).

% Advocate for expansive CA3 application based on humanitarian imperatives. Their customary law reading (icrc_customary_reading) is marginalized when states control the interpretive framework. Provide services in excluded conflicts without legal mandate, relying on consent of parties. Document protection gaps but cannot compel application.
narrative_ontology:constraint_stakeholder(common_article_3_scope__state_centric_reading, icrc_and_humanitarian_ngos, excluded,
    organized, biographical, mobile, global).

% Adjudicate threshold questions in specific cases (ICTY Tadić, ICC situations). Their jurisprudence has generally favored lower thresholds (expansive_reading influence), but lack enforcement power over state classification decisions in ongoing operations. Provide authoritative interpretations that states may reject or distinguish.
narrative_ontology:constraint_stakeholder(common_article_3_scope__state_centric_reading, international_courts_and_tribunals, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides legal certainty for states by establishing clear thresholds (intensity of violence, organization of non-state groups) that distinguish armed conflict from law enforcement, preventing IHL from swallowing domestic criminal law and human rights law.
% TRANSFER_FUNCTION: Moves humanitarian protection away from victims of low-intensity and non-threshold violence toward state operational discretion. The transfer is protection-for-discretion: states gain freedom from IHL constraints (detention rules, targeting standards, fair trial guarantees) in exchange for denying those protections to persons in excluded violence.
% ABSENT_VOICES: Victims of excluded violence (irregular combatants below threshold, civilians in low-intensity conflicts) have no standing in the interpretive process. Human rights advocates arguing for broader application are excluded from state-controlled treaty interpretation. The ICRC's customary law methodology is sidelined when states assert sovereign interpretive authority.
% DISAPPEARANCE_RATIONALE: If the intensity/organization thresholds vanished overnight, CA3 would apply to all organized armed violence. States would lose the law-enforcement classification for counterinsurgency/counterterrorism operations. Non-state groups would gain IHL protections. Military operations would require IHL compliance (distinction, proportionality, detention review). Human rights law would be displaced as lex specialis in more contexts. The global map of regulated conflicts would expand significantly.
% FOUNDING_PROBLEM: The 1949 Geneva Conventions needed to define the lower boundary of 'armed conflict not of an international character' to prevent IHL from applying to ordinary internal disturbances, riots, and isolated acts of violence — preserving the distinction between war and law enforcement.
% FOUNDING_PROBLEM_CORROBORATION: The ICRC's 1949-1950 commentaries and travaux préparatoires support a threshold function (corroboration from drafting history). However, the ICRC's 2016 Commentary and customary law study argue the founding problem is substantially solved and thresholds now function as protection gaps. States maintain the problem is live (citing terrorism, hybrid warfare). The discrepancy between drafting intent and contemporary state practice is documented in ILC reports and academic literature outside beneficiary circles.
narrative_ontology:disappearance_verdict(common_article_3_scope__state_centric_reading, world_rearranges).
narrative_ontology:founding_problem_status(common_article_3_scope__state_centric_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(common_article_3_scope__state_centric_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(common_article_3_scope__state_centric_reading, 'none', 1).
narrative_ontology:epsilon_provenance(common_article_3_scope__state_centric_reading, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness (0.68) is high because the constraint transfers substantial humanitarian protection value from powerless victims to institutional beneficiaries. Suppression (0.72) is high because the constraint's persistence depends on active state enforcement of classification boundaries — judicial decisions, military manuals, diplomatic pressure against expansive interpretations. Theater (0.45) is moderate: the legal certainty rationale is real but increasingly performs a cover function as thresholds become doctrinal tools for exclusion rather than genuine boundary-markers. Accessibility collapse (0.58) reflects that alternative interpretations (expansive, customary) exist and are authoritatively articulated but are structurally blocked from adoption by state interpretive monopoly. Resistance (0.55) is significant: international courts, ICRC, NGOs, and some states push back, but the constraint holds because the beneficiaries control the classification machinery.
 *
 * PERSPECTIVAL GAP:
 *   From the state seat, this is a rope: clear thresholds solve a genuine coordination problem (distinguishing war from crime). From the victim seats, this is a snare: the thresholds are manipulated to exclude precisely the conflicts where protection is most needed. The engine computes this divergence from the structural data — state power + arbitrage exit vs. victim powerlessness + trapped exit.
 *
 * DIRECTIONALITY LOGIC:
 *   State governments and military commands are structural beneficiaries (d near 0.0-0.2): they collect discretionary operational freedom, control the interpretive framework, and face no exit costs. Irregular combatants below threshold and civilians in low-intensity conflicts are full targets (d near 0.9-1.0): they bear the full protection loss, are trapped in the classification (cannot exit the state's legal characterization), and have zero power to contest it. Non-state armed groups excluded are constrained targets (d ~0.7): they bear costs but may have some organizational capacity to push toward threshold. ICRC/NGOs are excluded observers (d ~0.5): they advocate but lack structural leverage. International courts are analytical observers (d ~0.4): they interpret authoritatively but lack enforcement.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (distinguishing war from law enforcement) remains live but the solution has mutated. The original threshold function has been weaponized: states now design operations to stay below thresholds (intensity management, proxy forces, 'law enforcement' framing for military operations). The arrangement persists not because it solves the founding problem well, but because it solves a different problem for beneficiaries — maintaining IHL-free zones in counterinsurgency. This is classic mandatrophy: the mandate (thresholds for legal certainty) has atrophied into a tool for protection denial.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    threshold_manipulation_extent,
    'To what extent do states deliberately calibrate military operations to remain below CA3 thresholds (intensity management, proxy forces, law enforcement framing) rather than thresholds reflecting organic conflict dynamics?',
    'Comparative analysis of state counterinsurgency doctrine, operational records, and classification decisions across conflicts; correlation between threshold rhetoric and operational design.',
    'If thresholds are actively managed, the constraint is a designed exclusion mechanism (snare confirmed). If thresholds track organic conflict dynamics, the coordination function retains more weight (tangled_rope possible).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(threshold_manipulation_extent, empirical, 'Whether thresholds are organic conflict descriptors or state-calibrated exclusion tools.').

omega_variable(
    human_rights_law_substitution_gap,
    'Does human rights law (HRL) actually provide equivalent or superior protection in excluded low-intensity contexts, or does the CA3 gap create a protection vacuum that HRL cannot fill due to its different paradigm (law enforcement vs. conduct of hostilities)?',
    'Case-by-case comparison of protection outcomes in threshold-excluded conflicts: extrajudicial killings, detention conditions, fair trial access, collective punishment — under HRL vs. CA3 frameworks.',
    'If HRL fills the gap, extraction is lower (some protection substitutes). If HRL leaves a vacuum, extraction is higher and the snare classification strengthens.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(human_rights_law_substitution_gap, conceptual, 'Whether the human rights law fallback genuinely substitutes for CA3 protections in excluded contexts.').

omega_variable(
    state_centric_reading_legitimacy,
    'Is the state-centric reading a genuine interpretive position grounded in treaty text and drafting history, or a strategic deployment of interpretive authority to evade humanitarian obligations?',
    'Treaty interpretation analysis (VCLT Arts. 31-33): text, context, object and purpose, subsequent practice. Assessment of whether state practice supports or contradicts the reading.',
    'If genuine interpretation, the constraint has coordination legitimacy (tangled_rope element). If strategic evasion, it is pure extraction (snare confirmed). This is the core mandatrophy ambiguity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_centric_reading_legitimacy, conceptual, 'Whether the state-centric reading''s coordination claim is authentic or instrumental.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(common_article_3_scope__state_centric_reading, 1949, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comm_tr_t1949, common_article_3_scope__state_centric_reading, theater_ratio, 1949, 0.2).
narrative_ontology:measurement(comm_tr_t1977, common_article_3_scope__state_centric_reading, theater_ratio, 1977, 0.25).
narrative_ontology:measurement(comm_tr_t1995, common_article_3_scope__state_centric_reading, theater_ratio, 1995, 0.32).
narrative_ontology:measurement(comm_tr_t2001, common_article_3_scope__state_centric_reading, theater_ratio, 2001, 0.38).
narrative_ontology:measurement(comm_tr_t2011, common_article_3_scope__state_centric_reading, theater_ratio, 2011, 0.42).
narrative_ontology:measurement(comm_tr_t2024, common_article_3_scope__state_centric_reading, theater_ratio, 2024, 0.45).

% Extraction over time
narrative_ontology:measurement(comm_be_t1949, common_article_3_scope__state_centric_reading, base_extractiveness, 1949, 0.35).
narrative_ontology:measurement(comm_be_t1977, common_article_3_scope__state_centric_reading, base_extractiveness, 1977, 0.42).
narrative_ontology:measurement(comm_be_t1995, common_article_3_scope__state_centric_reading, base_extractiveness, 1995, 0.52).
narrative_ontology:measurement(comm_be_t2001, common_article_3_scope__state_centric_reading, base_extractiveness, 2001, 0.61).
narrative_ontology:measurement(comm_be_t2011, common_article_3_scope__state_centric_reading, base_extractiveness, 2011, 0.65).
narrative_ontology:measurement(comm_be_t2024, common_article_3_scope__state_centric_reading, base_extractiveness, 2024, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(comm_su_t1949, common_article_3_scope__state_centric_reading, suppression_requirement, 1949, 0.4).
narrative_ontology:measurement(comm_su_t1977, common_article_3_scope__state_centric_reading, suppression_requirement, 1977, 0.48).
narrative_ontology:measurement(comm_su_t1995, common_article_3_scope__state_centric_reading, suppression_requirement, 1995, 0.58).
narrative_ontology:measurement(comm_su_t2001, common_article_3_scope__state_centric_reading, suppression_requirement, 2001, 0.65).
narrative_ontology:measurement(comm_su_t2011, common_article_3_scope__state_centric_reading, suppression_requirement, 2011, 0.69).
narrative_ontology:measurement(comm_su_t2024, common_article_3_scope__state_centric_reading, suppression_requirement, 2024, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(common_article_3_scope__state_centric_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(common_article_3_scope__state_centric_reading, common_article_3_scope__expansive_human_rights_reading).
narrative_ontology:affects_constraint(common_article_3_scope__state_centric_reading, common_article_3_scope__icrc_customary_reading).
narrative_ontology:affects_constraint(common_article_3_scope__state_centric_reading, non_international_armed_conflict_classification).
narrative_ontology:affects_constraint(common_article_3_scope__state_centric_reading, human_rights_law_lex_specialis_displacement).

% DUAL FORMULATION NOTE:
% This constraint family (common_article_3_scope) decomposes the single treaty provision CA3 into three structurally distinct constraints with different ε values and beneficiary/victim structures. The state-centric reading (this story) has high extraction (0.68) and snare classification. The expansive_human_rights_reading has low extraction (~0.15) and rope classification (coordination floor). The icrc_customary_reading has moderate extraction (~0.35) and tangled_rope classification (coordination via customary law + extraction via state resistance). They are linked because each reading's classification is cited as evidence in the others' interpretive contests.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(common_article_3_scope__state_centric_reading, institutional, 0.15).
constraint_indexing:directionality_override(common_article_3_scope__state_centric_reading, organized, 0.25).
constraint_indexing:directionality_override(common_article_3_scope__state_centric_reading, powerless, 0.95).
constraint_indexing:directionality_override(common_article_3_scope__state_centric_reading, moderate, 0.7).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
