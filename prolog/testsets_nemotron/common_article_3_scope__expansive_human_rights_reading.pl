% ============================================================================
% CONSTRAINT STORY: common_article_3_scope__expansive_human_rights_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_common_article_3_scope__expansive_human_rights_reading, []).

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
 *   constraint_id: common_article_3_scope__expansive_human_rights_reading
 *   human_readable: CA3 Expansive Human Rights Reading: Universal Application to Organized Armed Violence
 *   domain: international_humanitarian_law
 *
 * SUMMARY:
 *   Common Article 3 of the Geneva Conventions establishes minimum
 *   humanitarian standards for 'armed conflict not of an international
 *   character.' The expansive human rights reading asserts that CA3 applies
 *   to ANY organized armed violence as a floor of protection, regardless of
 *   how parties classify the situation. This reading emerged from the
 *   historical reality that states routinely deny 'armed conflict' status to
 *   avoid IHL obligations (colonial conflicts, 'The Troubles,'
 *   counter-terrorism operations), creating protection gaps. The reading
 *   transfers compliance burdens onto state and non-state perpetrators of
 *   organized violence and extends protections to all detainees and affected
 *   civilians. It is structurally a tangled rope: it coordinates a universal
 *   protection floor (genuine coordination function) while extracting
 *   compliance costs from actors who previously avoided them through
 *   classification games (asymmetric extraction). Active enforcement is
 *   required — monitoring bodies, prosecutors, and diplomatic pressure
 *   sustain the reading against state resistance.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(common_article_3_scope__expansive_human_rights_reading, 0.78).
domain_priors:suppression_score(common_article_3_scope__expansive_human_rights_reading, 0.72).
domain_priors:theater_ratio(common_article_3_scope__expansive_human_rights_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(common_article_3_scope__expansive_human_rights_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(common_article_3_scope__expansive_human_rights_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(common_article_3_scope__expansive_human_rights_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(common_article_3_scope__expansive_human_rights_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(common_article_3_scope__expansive_human_rights_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(common_article_3_scope__expansive_human_rights_reading, tangled_rope).
narrative_ontology:human_readable(common_article_3_scope__expansive_human_rights_reading, "CA3 Expansive Human Rights Reading: Universal Application to Organized Armed Violence").
narrative_ontology:topic_domain(common_article_3_scope__expansive_human_rights_reading, "international_humanitarian_law").

domain_priors:requires_active_enforcement(common_article_3_scope__expansive_human_rights_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(common_article_3_scope__expansive_human_rights_reading, 'a9500915-ce35-46b6-a7d6-911d79607b29').
narrative_ontology:cs_kernel_codification('a9500915-ce35-46b6-a7d6-911d79607b29', formalized).
narrative_ontology:cs_authority_grounding('a9500915-ce35-46b6-a7d6-911d79607b29', extraction).
narrative_ontology:cs_interpretation_layer_present('a9500915-ce35-46b6-a7d6-911d79607b29').
narrative_ontology:cs_reading_relation('a9500915-ce35-46b6-a7d6-911d79607b29', common_article_3_scope__state_centric_reading, forecloses).
narrative_ontology:cs_reading_relation('a9500915-ce35-46b6-a7d6-911d79607b29', common_article_3_scope__icrc_customary_reading, coexists_with).
narrative_ontology:cs_axiom('a9500915-ce35-46b6-a7d6-911d79607b29', foundational, human_dignity_non_derogable_in_organized_violence).
narrative_ontology:cs_axiom_status(human_dignity_non_derogable_in_organized_violence, holdable).
narrative_ontology:cs_axiom_grounding('a9500915-ce35-46b6-a7d6-911d79607b29', human_dignity_non_derogable_in_organized_violence, deontological).
narrative_ontology:cs_axiom('a9500915-ce35-46b6-a7d6-911d79607b29', foundational, no_classification_escape_from_minimum_standards).
narrative_ontology:cs_axiom_status(no_classification_escape_from_minimum_standards, holdable).
narrative_ontology:cs_axiom_grounding('a9500915-ce35-46b6-a7d6-911d79607b29', no_classification_escape_from_minimum_standards, deontological).
narrative_ontology:cs_reference_frame('a9500915-ce35-46b6-a7d6-911d79607b29', common_article_3_universal_humanitarian_floor).
narrative_ontology:cs_drift_state('a9500915-ce35-46b6-a7d6-911d79607b29', post_911_counter_terrorism_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('a9500915-ce35-46b6-a7d6-911d79607b29', '').
narrative_ontology:cs_kernel_id(common_article_3_scope__expansive_human_rights_reading, common_article_3_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(common_article_3_scope__expansive_human_rights_reading, detainees_in_any_armed_violence).
narrative_ontology:constraint_beneficiary(common_article_3_scope__expansive_human_rights_reading, civilian_populations_in_any_armed_violence).
narrative_ontology:constraint_beneficiary(common_article_3_scope__expansive_human_rights_reading, international_human_rights_monitoring_bodies).
narrative_ontology:constraint_beneficiary(common_article_3_scope__expansive_human_rights_reading, prosecutors_of_international_crimes).
narrative_ontology:constraint_victim(common_article_3_scope__expansive_human_rights_reading, state_security_forces_in_low_intensity_operations).
narrative_ontology:constraint_victim(common_article_3_scope__expansive_human_rights_reading, non_state_armed_groups_in_low_intensity_operations).
narrative_ontology:constraint_victim(common_article_3_scope__expansive_human_rights_reading, state_governments_resisting_external_monitoring).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Persons deprived of liberty in any organized armed violence — including low-intensity conflicts, counter-terrorism operations, and situations states classify as law enforcement — receive CA3 protections: humane treatment, prohibition of violence to life and person, outrages upon personal dignity, and judicial guarantees. They cannot exit the constraint's protection; the constraint follows them regardless of how their captors classify the situation.
narrative_ontology:constraint_stakeholder(common_article_3_scope__expansive_human_rights_reading, detainees_in_any_armed_violence, beneficiary,
    powerless, biographical, trapped, global).

% Civilians caught in any organized armed violence benefit from CA3's floor protections: prohibition of collective punishments, hostage-taking, and humiliating treatment. The expansive reading extends these protections beyond traditional 'non-international armed conflict' thresholds to any organized armed violence, including situations states deny are conflicts.
narrative_ontology:constraint_stakeholder(common_article_3_scope__expansive_human_rights_reading, civilian_populations_in_any_armed_violence, beneficiary,
    powerless, biographical, trapped, global).

% UN treaty bodies, special rapporteurs, and regional human rights courts gain jurisdictional hooks to monitor and report on state conduct in low-intensity violence. The expansive reading treats their mandate as engaged whenever organized armed violence exists, regardless of state classification.
narrative_ontology:constraint_stakeholder(common_article_3_scope__expansive_human_rights_reading, international_human_rights_monitoring_bodies, beneficiary,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(common_article_3_scope__expansive_human_rights_reading, international_human_rights_monitoring_bodies, agenda_setter).

% ICC, national universal jurisdiction prosecutors, and hybrid tribunals gain a broader predicate for war crimes charges. CA3 violations in any organized armed violence become prosecutable without proving the conflict meets traditional NIAC thresholds. The reading expands the universe of prosecutable conduct.
narrative_ontology:constraint_stakeholder(common_article_3_scope__expansive_human_rights_reading, prosecutors_of_international_crimes, beneficiary,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(common_article_3_scope__expansive_human_rights_reading, prosecutors_of_international_crimes, agenda_setter).

% Military, police, and intelligence units conducting counter-insurgency, counter-terrorism, or internal security operations face external legal standards they previously avoided by classifying operations as law enforcement. They bear compliance costs: training, oversight, rules of engagement revision, and prosecution risk for conduct that was previously 'domestic law enforcement.' Exit is constrained — they cannot leave their state role, and the reading follows the violence, not the classification.
narrative_ontology:constraint_stakeholder(common_article_3_scope__expansive_human_rights_reading, state_security_forces_in_low_intensity_operations, payer,
    institutional, biographical, constrained, national).

% Rebel groups, militias, and criminal organizations with sufficient organization face CA3 obligations in situations they and states deny are 'armed conflicts.' They bear compliance costs and prosecution exposure without the institutional capacity of states. Exit is constrained — they are bound by the violence they perpetrate, not by consent to the constraint.
narrative_ontology:constraint_stakeholder(common_article_3_scope__expansive_human_rights_reading, non_state_armed_groups_in_low_intensity_operations, payer,
    organized, biographical, constrained, national).

% Governments that classify their security operations as law enforcement or counter-terrorism (not armed conflict) face international monitoring, reporting obligations, and potential prosecution they previously avoided. They bear diplomatic, legal, and political costs. Exit is constrained — they cannot opt out of the international legal order, but they resist the reading's application through non-cooperation and alternative legal framing.
narrative_ontology:constraint_stakeholder(common_article_3_scope__expansive_human_rights_reading, state_governments_resisting_external_monitoring, payer,
    institutional, generational, constrained, national).

% ICRC and scholars tracking customary IHL occupy an analytical seat: they document state practice and opinio juris on CA3 scope. The expansive reading creates pressure on their methodology — if CA3 applies universally to organized armed violence, the customary law threshold inquiry becomes secondary to the human rights floor. They neither collect nor pay but their epistemic authority is contested.
narrative_ontology:constraint_stakeholder(common_article_3_scope__expansive_human_rights_reading, icrc_and_customary_law_scholars, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a universal, non-derogable floor of humanitarian protections that activates whenever organized armed violence exists, regardless of how parties classify the situation. Solves the coordination problem of 'classification gaps' where states deny conflict status to avoid IHL obligations, leaving victims unprotected.
% TRANSFER_FUNCTION: Transfers legal protection and monitoring access from state security forces and non-state armed groups to detainees, civilians, and international oversight bodies. The constraint moves the burden of compliance onto perpetrators of organized violence and moves the benefit of protection to all affected persons, irrespective of conflict classification.
% ABSENT_VOICES: States that exclusively use law enforcement paradigms for organized violence (e.g., certain counter-terrorism frameworks) and non-state groups that reject any external legal constraints. These actors are structurally excluded from the reading's framework — they would object to the universal application but are not seated in the human rights monitoring architecture.
% DISAPPEARANCE_RATIONALE: If this reading vanished overnight, states would revert to classification-based IHL thresholds, leaving detainees and civilians in low-intensity violence without CA3 protections. International monitoring bodies would lose jurisdictional hooks in these situations. Prosecutors would face higher thresholds for war crimes charges. The protection architecture for millions in 'unacknowledged' conflicts would collapse.
% FOUNDING_PROBLEM: The classification gap in IHL: states deny 'armed conflict' status to avoid Geneva Convention obligations, creating protection vacuums for detainees and civilians in organized violence that falls below traditional NIAC thresholds (e.g., colonial wars, counter-insurgency, 'troubles,' counter-terrorism operations).
% FOUNDING_PROBLEM_CORROBORATION: ICRC commentaries (1987, 2016), UN Human Rights Committee General Comment No. 31, European Court of Human Rights jurisprudence (e.g., Hassan v UK), and Inter-American Court advisory opinions all attest that the classification gap persists and the human rights floor remains necessary. No major state or IHL authority asserts the gap is resolved.
narrative_ontology:disappearance_verdict(common_article_3_scope__expansive_human_rights_reading, world_rearranges).
narrative_ontology:founding_problem_status(common_article_3_scope__expansive_human_rights_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(common_article_3_scope__expansive_human_rights_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(common_article_3_scope__expansive_human_rights_reading, 'none', 1).
narrative_ontology:epsilon_provenance(common_article_3_scope__expansive_human_rights_reading, 0.78, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(common_article_3_scope__expansive_human_rights_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(common_article_3_scope__expansive_human_rights_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(common_article_3_scope__expansive_human_rights_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.78) reflects the substantial compliance burden imposed on state and non-state actors who previously operated in classification gaps. The reading extracts monitoring access, legal liability, and behavioral modification from these actors. Suppression (0.72) is high because the reading's persistence depends on active enforcement by international bodies against resistant states — without external pressure, states revert to classification denials. Theater ratio (0.45) is moderate: some monitoring activity is performative (reports without consequences), but prosecutions and diplomatic pressure are real. Accessibility collapse (0.35) is moderate — alternative legal frameworks (IHRL, domestic law) partially fill gaps but lack CA3's specific protections. Resistance (0.68) is high: states actively contest the reading's scope through legal arguments, non-cooperation, and alternative frameworks.
 *
 * PERSPECTIVAL GAP:
 *   From the detainee/civilian seat: the constraint is a genuine coordination mechanism delivering real protection — a rope. From the state security force seat: the constraint is extractive enforcement of standards they never consented to in 'law enforcement' operations — a snare. From the monitoring body seat: the constraint is a coordination tool enabling their mandate — a rope. The engine computes this divergence from the structural data. The claimed tangled_rope type reflects the structural reality: both coordination and extraction are real and simultaneous.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (detainees, civilians, monitoring bodies, prosecutors) have trapped or analytical exit — they cannot exit the protection framework, and analysts choose this reading. Payers (state security forces, non-state groups, resistant governments) have constrained exit — they are bound by their role in organized violence, not by consent. The engine will compute high effective extraction for payers (d near 1.0) and low/negative for beneficiaries (d near 0.0). The constraint's global scope amplifies extraction for payers across jurisdictions.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (classification gaps) remains live — states still deny conflict status (e.g., Russia in Ukraine pre-2022, various counter-terrorism operations). The reading has not atrophied; its mandate expands as new classification gaps emerge. However, theater ratio growth suggests some monitoring has become performative. The constraint is not a piton — active enforcement and real prosecutions demonstrate functional vitality.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_foreclosure_state_centric,
    'Does the expansive human rights reading logically foreclose the state-centric reading within a single legal framework, or can a framework hold both as alternative interpretive options?',
    'Analysis of whether any international court or treaty body has simultaneously applied threshold-based NIAC classification for some purposes while applying universal CA3 for others. If such dual application exists, foreclosure is not structural.',
    'If foreclosure holds, the kernel has a genuine structural split — frameworks must choose one reading. If coexistence holds, the kernel permits parallel readings across different institutional contexts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_foreclosure_state_centric, conceptual, 'Whether the expansive reading''s core premise (universal application to organized armed violence) logically excludes threshold-based classification within one framework.').

omega_variable(
    customary_law_relevance_under_universal_application,
    'If CA3 applies universally to organized armed violence, does the customary IHL threshold inquiry (ICRC customary reading) become practically irrelevant, or does it retain independent operative force?',
    'Track whether courts and monitoring bodies still engage customary NIAC threshold analysis when the expansive reading is available. If they do, the customary reading retains independent force; if they bypass it entirely, the expansive reading has structurally displaced it.',
    'If the customary reading is displaced, the kernel''s structure shifts from three readings to two. If it persists, the three-reading structure is stable.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(customary_law_relevance_under_universal_application, empirical, 'Whether the expansive reading structurally displaces the customary law reading in practice.').

omega_variable(
    suppression_mechanism_state_resistance,
    'Is the measured suppression (0.72) primarily structural (state non-cooperation, legal barriers to monitoring) or does it include internalized suppression (state security forces internalizing that certain populations are outside protections)?',
    'Compare suppression levels in situations with active monitoring access vs. situations where states deny all access. If suppression persists even with access, internalized component exists.',
    'If internalized suppression is significant, the constraint''s effective suppression is higher than structural measures suggest — the constraint operates partly through the perpetrators'' own cognitive frameworks.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_state_resistance, empirical, 'Structural vs. internalized suppression in state resistance to the expansive reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(common_article_3_scope__expansive_human_rights_reading, 1949, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comm_tr_t1949, common_article_3_scope__expansive_human_rights_reading, theater_ratio, 1949, 0.1).
narrative_ontology:measurement(comm_tr_t1977, common_article_3_scope__expansive_human_rights_reading, theater_ratio, 1977, 0.15).
narrative_ontology:measurement(comm_tr_t1995, common_article_3_scope__expansive_human_rights_reading, theater_ratio, 1995, 0.25).
narrative_ontology:measurement(comm_tr_t2001, common_article_3_scope__expansive_human_rights_reading, theater_ratio, 2001, 0.35).
narrative_ontology:measurement(comm_tr_t2011, common_article_3_scope__expansive_human_rights_reading, theater_ratio, 2011, 0.4).
narrative_ontology:measurement(comm_tr_t2024, common_article_3_scope__expansive_human_rights_reading, theater_ratio, 2024, 0.45).

% Extraction over time
narrative_ontology:measurement(comm_be_t1949, common_article_3_scope__expansive_human_rights_reading, base_extractiveness, 1949, 0.15).
narrative_ontology:measurement(comm_be_t1977, common_article_3_scope__expansive_human_rights_reading, base_extractiveness, 1977, 0.35).
narrative_ontology:measurement(comm_be_t1995, common_article_3_scope__expansive_human_rights_reading, base_extractiveness, 1995, 0.55).
narrative_ontology:measurement(comm_be_t2001, common_article_3_scope__expansive_human_rights_reading, base_extractiveness, 2001, 0.68).
narrative_ontology:measurement(comm_be_t2011, common_article_3_scope__expansive_human_rights_reading, base_extractiveness, 2011, 0.72).
narrative_ontology:measurement(comm_be_t2024, common_article_3_scope__expansive_human_rights_reading, base_extractiveness, 2024, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(comm_su_t1949, common_article_3_scope__expansive_human_rights_reading, suppression_requirement, 1949, 0.2).
narrative_ontology:measurement(comm_su_t1977, common_article_3_scope__expansive_human_rights_reading, suppression_requirement, 1977, 0.35).
narrative_ontology:measurement(comm_su_t1995, common_article_3_scope__expansive_human_rights_reading, suppression_requirement, 1995, 0.5).
narrative_ontology:measurement(comm_su_t2001, common_article_3_scope__expansive_human_rights_reading, suppression_requirement, 2001, 0.62).
narrative_ontology:measurement(comm_su_t2011, common_article_3_scope__expansive_human_rights_reading, suppression_requirement, 2011, 0.68).
narrative_ontology:measurement(comm_su_t2024, common_article_3_scope__expansive_human_rights_reading, suppression_requirement, 2024, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(common_article_3_scope__expansive_human_rights_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(common_article_3_scope__expansive_human_rights_reading, 0.12).
narrative_ontology:affects_constraint(common_article_3_scope__expansive_human_rights_reading, common_article_3_scope__state_centric_reading).
narrative_ontology:affects_constraint(common_article_3_scope__expansive_human_rights_reading, common_article_3_scope__icrc_customary_reading).
narrative_ontology:affects_constraint(common_article_3_scope__expansive_human_rights_reading, niac_threshold_classification).
narrative_ontology:affects_constraint(common_article_3_scope__expansive_human_rights_reading, ihl_ihrl_interplay_in_low_intensity_violence).

% DUAL FORMULATION NOTE:
% This reading and the state_centric_reading form a foreclosure pair within any single legal framework. The icrc_customary_reading coexists with both but faces displacement pressure from the expansive reading's universal application.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(common_article_3_scope__expansive_human_rights_reading, institutional, 0.85).
constraint_indexing:directionality_override(common_article_3_scope__expansive_human_rights_reading, organized, 0.9).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
