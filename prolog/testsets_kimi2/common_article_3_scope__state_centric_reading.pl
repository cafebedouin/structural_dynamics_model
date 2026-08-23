% ============================================================================
% CONSTRAINT STORY: common_article_3_scope__state_centric_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
 *   domain: international_humanitarian_law
 *
 * SUMMARY:
 *   Common Article 3 of the 1949 Geneva Conventions provides minimum
 *   humanitarian guarantees for non-international armed conflicts. The
 *   state-centric reading of CA3 scope restricts its application to conflicts
 *   meeting intensity and organization thresholds (the Tadic criteria),
 *   excluding low-level violence, internal disturbances, and law enforcement
 *   operations. This reading is contested by expansive human rights
 *   approaches that would apply CA3 to any organized armed violence, and by
 *   ICRC customary-law approaches that track evolving state practice. The
 *   state-centric reading structurally benefits states by preserving
 *   operational discretion and domestic legal frameworks, while irregular
 *   combatants and civilians in sub-threshold conflicts are excluded from
 *   CA3's protective regime. The constraint is authored as a kernel reading
 *   with siblings expansive_human_rights_reading and icrc_customary_reading.
 *
 * KEY AGENTS:
 *   - state_governments: Primary agenda-setter (institutional/arbitrage) â classifies conflicts, retains operational discretion
 *   - irregular_combatants: Primary target (powerless/trapped) â excluded from CA3 protections by threshold classification
 *   - civilians_in_unclassified_conflicts: Secondary target (powerless/trapped) â lack minimum guarantees in sub-threshold zones
 *   - human_rights_advocacy_networks: Excluded voice (moderate/constrained) â argues for floor-of-rights application without formal standing
 *   - international_criminal_tribunals: Beneficiary (institutional/analytical) â applies clear threshold test for jurisdiction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(common_article_3_scope__state_centric_reading, 0.72).
domain_priors:suppression_score(common_article_3_scope__state_centric_reading, 0.78).
domain_priors:theater_ratio(common_article_3_scope__state_centric_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(common_article_3_scope__state_centric_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(common_article_3_scope__state_centric_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(common_article_3_scope__state_centric_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(common_article_3_scope__state_centric_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(common_article_3_scope__state_centric_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(common_article_3_scope__state_centric_reading, tangled_rope).
narrative_ontology:human_readable(common_article_3_scope__state_centric_reading, "State-Centric Threshold Reading of Common Article 3 Scope").
narrative_ontology:topic_domain(common_article_3_scope__state_centric_reading, "international_humanitarian_law").

domain_priors:requires_active_enforcement(common_article_3_scope__state_centric_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(common_article_3_scope__state_centric_reading, 'eff47bb6-1ebf-4422-aeac-0c3eb4d47bab').
narrative_ontology:cs_kernel_codification('eff47bb6-1ebf-4422-aeac-0c3eb4d47bab', formalized).
narrative_ontology:cs_authority_grounding('eff47bb6-1ebf-4422-aeac-0c3eb4d47bab', lineage).
narrative_ontology:cs_interpretation_layer_present('eff47bb6-1ebf-4422-aeac-0c3eb4d47bab').
narrative_ontology:cs_reading_relation('eff47bb6-1ebf-4422-aeac-0c3eb4d47bab', common_article_3_scope__expansive_human_rights_reading, forecloses).
narrative_ontology:cs_reading_relation('eff47bb6-1ebf-4422-aeac-0c3eb4d47bab', common_article_3_scope__icrc_customary_reading, influences).
narrative_ontology:cs_axiom('eff47bb6-1ebf-4422-aeac-0c3eb4d47bab', foundational, intensity_organization_thresholds_mandatory).
narrative_ontology:cs_axiom_status(intensity_organization_thresholds_mandatory, holdable).
narrative_ontology:cs_axiom_grounding('eff47bb6-1ebf-4422-aeac-0c3eb4d47bab', intensity_organization_thresholds_mandatory, conventional).
narrative_ontology:cs_axiom('eff47bb6-1ebf-4422-aeac-0c3eb4d47bab', foundational, state_primacy_in_conflict_classification).
narrative_ontology:cs_axiom_status(state_primacy_in_conflict_classification, holdable).
narrative_ontology:cs_axiom_grounding('eff47bb6-1ebf-4422-aeac-0c3eb4d47bab', state_primacy_in_conflict_classification, conventional).
narrative_ontology:cs_reference_frame('eff47bb6-1ebf-4422-aeac-0c3eb4d47bab', state_consent_based_ihl_framework).
narrative_ontology:cs_drift_state('eff47bb6-1ebf-4422-aeac-0c3eb4d47bab', contemporary_human_rights_expansion, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('eff47bb6-1ebf-4422-aeac-0c3eb4d47bab', '').
narrative_ontology:cs_kernel_id(common_article_3_scope__state_centric_reading, common_article_3_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(common_article_3_scope__state_centric_reading, state_governments).
narrative_ontology:constraint_beneficiary(common_article_3_scope__state_centric_reading, international_criminal_tribunals).
narrative_ontology:constraint_victim(common_article_3_scope__state_centric_reading, irregular_combatants).
narrative_ontology:constraint_victim(common_article_3_scope__state_centric_reading, civilians_in_unclassified_conflicts).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Classify internal violence as either armed conflict triggering CA3 or law enforcement falling below the threshold. They define intensity and organization criteria through national practice, military manuals, and diplomatic argumentation. They retain maximum operational discretion in sub-threshold situations and resist external classification that would expand CA3 obligations.
narrative_ontology:constraint_stakeholder(common_article_3_scope__state_centric_reading, state_governments, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(common_article_3_scope__state_centric_reading, state_governments, beneficiary).

% Participate in organized armed violence that state governments classify as below the intensity or organization threshold. They are denied CA3 protections including humane treatment, fair trial guarantees, and safeguards against torture and summary execution. They cannot unilaterally elevate the conflict classification to trigger protections.
narrative_ontology:constraint_stakeholder(common_article_3_scope__state_centric_reading, irregular_combatants, payer,
    powerless, immediate, trapped, local).

% Inhabit territories experiencing sustained low-level violence, internal disturbances, or tensions that states classify below the CA3 threshold. They lack the explicit minimum humanitarian guarantees against murder, mutilation, torture, and hostage-taking that CA3 would provide, and may be subjected to domestic security operations without IHL oversight.
narrative_ontology:constraint_stakeholder(common_article_3_scope__state_centric_reading, civilians_in_unclassified_conflicts, payer,
    powerless, immediate, trapped, local).

% Argue that CA3 should apply as a floor of minimum standards to all organized armed violence regardless of threshold classification. They are structurally excluded from the formal state-centric interpretive framework that sets intensity and organization requirements, and their alternative readings are treated as political rather than legal arguments in inter-state forums.
narrative_ontology:constraint_stakeholder(common_article_3_scope__state_centric_reading, human_rights_advocacy_networks, excluded,
    moderate, generational, constrained, global).

% Apply the Tadic threshold test to determine jurisdiction over non-international armed conflicts. They benefit from having concrete, state-accepted legal criteria to establish the applicability of CA3 and avoid direct confrontations with state sovereignty over conflict classification.
narrative_ontology:constraint_stakeholder(common_article_3_scope__state_centric_reading, international_criminal_tribunals, beneficiary,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(common_article_3_scope__state_centric_reading, state_governments).
narrative_ontology:fixing_cost_class(common_article_3_scope__state_centric_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes clear jurisdictional thresholds that separate armed conflict subject to international humanitarian law from internal disturbances and law enforcement subject to domestic jurisdiction, preserving state sovereignty and the state-consent foundation of the Geneva Conventions.
% TRANSFER_FUNCTION: Moves legal protections, fair trial guarantees, and minimum humanitarian safeguards away from irregular combatants and civilians in low-intensity or unclassified conflicts toward state governments as operational discretion and toward international criminal tribunals as jurisdictional clarity.
% ABSENT_VOICES: Irregular combatants excluded by the threshold, civilians in zones of persistent low-level violence, and human rights advocates arguing for floor-of-rights application are structurally absent from the formal state-centric interpretive framework that sets the thresholds.
% DISAPPEARANCE_RATIONALE: If the state-centric threshold vanished and CA3 applied broadly to all organized armed violence, states would lose significant operational discretion, more internal conflicts would be classified as non-international armed conflicts triggering IHL obligations, detainees would gain minimum guarantees, and the sovereignty-humanitarian oversight balance would shift toward external legal accountability.
% FOUNDING_PROBLEM: To establish a clear legal boundary between armed conflict subject to international humanitarian law and internal disturbances or law enforcement subject to domestic jurisdiction, ensuring state consent to the Geneva Conventions by respecting sovereignty over internal security.
% FOUNDING_PROBLEM_CORROBORATION: States and the ICRC Commentary attest the boundary was originally necessary to preserve IHL's integrity and state consent. However, contemporary UN human rights mechanisms, special rapporteurs, and independent legal scholars from outside the state-beneficiary set attest the threshold has become a tool for systematic exclusion; corroboration is split across institutional lines.
narrative_ontology:disappearance_verdict(common_article_3_scope__state_centric_reading, world_rearranges).
narrative_ontology:founding_problem_status(common_article_3_scope__state_centric_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(common_article_3_scope__state_centric_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
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
 *   Extractiveness (0.72) is high because the threshold doctrine systematically withholds humanitarian legal protections from vulnerable populations in conflicts that states classify as sub-threshold, transferring the costs of sovereignty preservation to those populations. Suppression (0.78) is higher still because the constraint's persistence depends on actively resisting expansive human rights interpretations and excluding alternative legal frameworks from application. Theater_ratio (0.45) reflects the elaborate legal ritual around intensity and organization assessment, which functions partly as genuine legal analysis and partly as performative sovereignty defense. Accessibility_collapse (0.65) captures that while human rights law persists as an alternative, it is significantly weaker in providing the specific protections CA3 guarantees. Resistance (0.60) reflects sustained pressure from human rights bodies, UN mechanisms, and some judicial actors to lower or eliminate the thresholds. The temporal series shows extraction and suppression rising over the interval as human rights norms developed and the state-centric reading was increasingly mobilized to exclude them; all metrics share the 0-75 time grid.
 *
 * PERSPECTIVAL GAP:
 *   The state_governments seat experiences the constraint as necessary coordination that preserves international legal order and state consent; the irregular_combatants and civilians seats experience the same structure as violent exclusion from legal protection. The international_criminal_tribunals seat experiences it as a workable jurisdictional test. The engine computes this divergence from the structural data: identical legal provisions produce opposite directionalities depending on whether the agent classifies or is classified.
 *
 * DIRECTIONALITY LOGIC:
 *   State_governments are structural beneficiaries (d near 0.0) because the constraint subsidizes their operational discretion and sovereignty. International_criminal_tribunals are mild beneficiaries (d ~0.2) because the test provides jurisdictional clarity. Irregular_combatants and civilians_in_unclassified_conflicts are full targets (d near 1.0) because the constraint extracts legal protections from them and they have trapped exit â they cannot unilaterally reclassify the conflicts they inhabit. Human_rights_advocacy_networks are excluded observers with constrained exit (d ~0.5 symmetrically excluded).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â distinguishing armed conflict from law enforcement to preserve state consent â was genuinely live in 1949. It is now contested: states assert it remains live, while external observers document protection gaps. The (founding_problem_status = contested x disappearance_verdict = world_rearranges) mismatch signals that the arrangement may have shifted from scaffold-like transitional coordination toward persistent extraction, but the constraint retains enough genuine coordination function (preventing total IHL dilution) to avoid snare classification. The temporal measurements show extraction accumulation over time, which the T17 abductive trigger would flag for investigation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    threshold_necessity_vs_exclusion,
    'Is the intensity and organization threshold a necessary structural feature of international humanitarian law to secure state consent, or is it a constructed barrier that extracts protections from vulnerable populations?',
    'Comparative analysis of state practice in jurisdictions that have adopted expansive human rights readings versus state-centric readings; measurement of protection gaps in sub-threshold conflicts.',
    'If the threshold is necessary, the constraint remains a tangled rope with genuine coordination function; if purely constructed for exclusion, it reclassifies toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(threshold_necessity_vs_exclusion, conceptual, 'Whether the threshold doctrine is structurally necessary or purely extractive').

omega_variable(
    customary_evolution_pressure,
    'Does evolving customary international law documented by the ICRC support expanding CA3 beyond the state-centric threshold, rendering this reading increasingly extractive?',
    'Tracking state opinio juris and practice over time; monitoring domestic court rulings that apply CA3 to low-intensity violence.',
    'If customary law is expanding, the state-centric reading suppresses an emerging norm and functions more as active extraction than coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(customary_evolution_pressure, empirical, 'Whether customary law evolution undermines the state-centric threshold').

omega_variable(
    kernel_reading_contest,
    'This constraint is one reading of the contested kernel common_article_3_scope. The sibling readings (expansive_human_rights_reading, icrc_customary_reading) would change the beneficiary/victim structure and scope. What classification would change if the expansive reading were adopted?',
    'Comparison across the constraint family; the expansive reading would likely classify as rope or scaffold (genuine coordination with broader beneficiaries) while this reading extracts from excluded groups.',
    'The kernel''s structural indeterminacy means the constraint''s type depends on which reading dominates the interpretive field.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Kernel reading contest and structural indeterminacy').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(common_article_3_scope__state_centric_reading, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ca3_state_tr_t0, common_article_3_scope__state_centric_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(ca3_state_tr_t15, common_article_3_scope__state_centric_reading, theater_ratio, 15, 0.25).
narrative_ontology:measurement(ca3_state_tr_t30, common_article_3_scope__state_centric_reading, theater_ratio, 30, 0.3).
narrative_ontology:measurement(ca3_state_tr_t45, common_article_3_scope__state_centric_reading, theater_ratio, 45, 0.4).
narrative_ontology:measurement(ca3_state_tr_t60, common_article_3_scope__state_centric_reading, theater_ratio, 60, 0.48).
narrative_ontology:measurement(ca3_state_tr_t75, common_article_3_scope__state_centric_reading, theater_ratio, 75, 0.45).

% Extraction over time
narrative_ontology:measurement(ca3_state_be_t0, common_article_3_scope__state_centric_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(ca3_state_be_t15, common_article_3_scope__state_centric_reading, base_extractiveness, 15, 0.48).
narrative_ontology:measurement(ca3_state_be_t30, common_article_3_scope__state_centric_reading, base_extractiveness, 30, 0.55).
narrative_ontology:measurement(ca3_state_be_t45, common_article_3_scope__state_centric_reading, base_extractiveness, 45, 0.62).
narrative_ontology:measurement(ca3_state_be_t60, common_article_3_scope__state_centric_reading, base_extractiveness, 60, 0.68).
narrative_ontology:measurement(ca3_state_be_t75, common_article_3_scope__state_centric_reading, base_extractiveness, 75, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(ca3_state_su_t0, common_article_3_scope__state_centric_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(ca3_state_su_t15, common_article_3_scope__state_centric_reading, suppression_requirement, 15, 0.42).
narrative_ontology:measurement(ca3_state_su_t30, common_article_3_scope__state_centric_reading, suppression_requirement, 30, 0.5).
narrative_ontology:measurement(ca3_state_su_t45, common_article_3_scope__state_centric_reading, suppression_requirement, 45, 0.6).
narrative_ontology:measurement(ca3_state_su_t60, common_article_3_scope__state_centric_reading, suppression_requirement, 60, 0.72).
narrative_ontology:measurement(ca3_state_su_t75, common_article_3_scope__state_centric_reading, suppression_requirement, 75, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(common_article_3_scope__state_centric_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(common_article_3_scope__state_centric_reading, expansive_human_rights_reading).
narrative_ontology:affects_constraint(common_article_3_scope__state_centric_reading, icrc_customary_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the common_article_3_scope kernel. It is structurally paired with expansive_human_rights_reading and icrc_customary_reading as alternative indexical commitments to the same legal text. The epsilon-invariance principle requires separate stories because the readings have different beneficiary/victim structures and different epsilon values.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
