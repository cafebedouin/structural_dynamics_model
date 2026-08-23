% ============================================================================
% CONSTRAINT STORY: supermajority_threshold__adaptive_gradient_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_supermajority_threshold__adaptive_gradient_reading, []).

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
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: supermajority_threshold__adaptive_gradient_reading
 *   human_readable: Supermajority Threshold (Adaptive Gradient Reading)
 *   domain: constitutional_theory/political_economy/institutional_design
 *
 * SUMMARY:
 *   The adaptive gradient reading views the supermajority threshold not as an
 *   intrinsic safeguard or a permanent veto, but as a functional tool whose
 *   legitimacy depends on continuous calibration to empirically measurable
 *   social consensus formation rates and the reversibility costs of
 *   constitutional decisions. This reading rejects both the consensus
 *   safeguard framing (which treats the threshold as a fixed expression of
 *   deep democratic will) and the minoritarian veto framing (which treats it
 *   as an entrenched privilege). Instead, it sees the threshold as a
 *   parameter that must be evidence-tuned: too low and it fails to prevent
 *   instability (rope tension), too high and it becomes a snare extracting
 *   consensus from majorities. The constraint's extraction is moderate
 *   because current thresholds in many systems are partially miscalibrated;
 *   suppression is moderate because the threshold is enforced by courts but
 *   alternatives (informal amendment, judicial interpretation) persist;
 *   theater is low because the threshold performs a genuine coordination
 *   function when calibrated.
 *
 * KEY AGENTS:
 *   - majority_factions: Primary payer (powerful/constrained) — bears cost of blocked reforms
 *   - minority_factions: Primary beneficiary (moderate/constrained) — gains veto protection
 *   - the_polity: Coordination beneficiary (organized/mobile) — gains systemic stability
 *   - constitutional_courts: Agenda setter (institutional/analytical) — administers and can tune threshold
 *   - future_generations: Excluded (powerless/trapped) — bears long-run reversibility costs
 *   - comparative_constitutional_scholars: Observer (analytical/analytical) — provides calibration evidence
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(supermajority_threshold__adaptive_gradient_reading, 0.35).
domain_priors:suppression_score(supermajority_threshold__adaptive_gradient_reading, 0.45).
domain_priors:theater_ratio(supermajority_threshold__adaptive_gradient_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(supermajority_threshold__adaptive_gradient_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(supermajority_threshold__adaptive_gradient_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(supermajority_threshold__adaptive_gradient_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(supermajority_threshold__adaptive_gradient_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(supermajority_threshold__adaptive_gradient_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(supermajority_threshold__adaptive_gradient_reading, rope).
narrative_ontology:human_readable(supermajority_threshold__adaptive_gradient_reading, "Supermajority Threshold (Adaptive Gradient Reading)").
narrative_ontology:topic_domain(supermajority_threshold__adaptive_gradient_reading, "constitutional_theory/political_economy/institutional_design").

domain_priors:requires_active_enforcement(supermajority_threshold__adaptive_gradient_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(supermajority_threshold__adaptive_gradient_reading, '42c7ff66-3093-4cd6-bb92-ad6b12600b71').
narrative_ontology:cs_kernel_codification('42c7ff66-3093-4cd6-bb92-ad6b12600b71', formalized).
narrative_ontology:cs_authority_grounding('42c7ff66-3093-4cd6-bb92-ad6b12600b71', expertise).
narrative_ontology:cs_interpretation_layer_present('42c7ff66-3093-4cd6-bb92-ad6b12600b71').
narrative_ontology:cs_reading_relation('42c7ff66-3093-4cd6-bb92-ad6b12600b71', supermajority_threshold__consensus_safeguard_reading, influences).
narrative_ontology:cs_reading_relation('42c7ff66-3093-4cd6-bb92-ad6b12600b71', supermajority_threshold__minoritarian_veto_reading, coexists_with).
narrative_ontology:cs_axiom('42c7ff66-3093-4cd6-bb92-ad6b12600b71', foundational, threshold_legitimacy_requires_empirical_calibration).
narrative_ontology:cs_axiom_status(threshold_legitimacy_requires_empirical_calibration, holdable).
narrative_ontology:cs_axiom_grounding('42c7ff66-3093-4cd6-bb92-ad6b12600b71', threshold_legitimacy_requires_empirical_calibration, empirically_contingent).
narrative_ontology:cs_axiom('42c7ff66-3093-4cd6-bb92-ad6b12600b71', secondary, constitutional_stability_and_adaptability_are_measurable).
narrative_ontology:cs_axiom_status(constitutional_stability_and_adaptability_are_measurable, holdable).
narrative_ontology:cs_axiom_grounding('42c7ff66-3093-4cd6-bb92-ad6b12600b71', constitutional_stability_and_adaptability_are_measurable, empirically_contingent).
narrative_ontology:cs_reference_frame('42c7ff66-3093-4cd6-bb92-ad6b12600b71', evidence_based_calibration_framework).
narrative_ontology:cs_drift_state('42c7ff66-3093-4cd6-bb92-ad6b12600b71', contemporary_constitutional_practice, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('42c7ff66-3093-4cd6-bb92-ad6b12600b71', '2026-08-15T14:30:00Z').
narrative_ontology:cs_kernel_id(supermajority_threshold__adaptive_gradient_reading, supermajority_threshold).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(supermajority_threshold__adaptive_gradient_reading, the_polity).
narrative_ontology:constraint_beneficiary(supermajority_threshold__adaptive_gradient_reading, democratic_process).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(supermajority_threshold__adaptive_gradient_reading, minority_factions).
narrative_ontology:constraint_victim(supermajority_threshold__adaptive_gradient_reading, majority_factions).
narrative_ontology:constraint_vindicates(supermajority_threshold__adaptive_gradient_reading, constitutional_stability_requires_calibrated_thresholds).
narrative_ontology:constraint_vindicates(supermajority_threshold__adaptive_gradient_reading, reversibility_costs_must_inform_amendment_rules).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Seek to amend the constitution through simple majoritarian procedures but are blocked by the supermajority threshold. They bear the cost of delayed or prevented reforms. Exit involves constitutional crisis or extra-legal action.
narrative_ontology:constraint_stakeholder(supermajority_threshold__adaptive_gradient_reading, majority_factions, payer,
    powerful, biographical, constrained, national).

% Use the supermajority threshold to block changes that threaten their interests. They benefit from the veto power the threshold confers. Exit is difficult because their protection is embedded in the constitutional structure.
narrative_ontology:constraint_stakeholder(supermajority_threshold__adaptive_gradient_reading, minority_factions, beneficiary,
    moderate, biographical, constrained, national).

% Gains systemic stability and protection against transient majoritarian passions. The threshold acts as a coordination mechanism ensuring changes reflect deep consensus. Exit is not meaningful for the polity as a whole.
narrative_ontology:constraint_stakeholder(supermajority_threshold__adaptive_gradient_reading, the_polity, beneficiary,
    organized, generational, mobile, national).

% Interpret and enforce the supermajority threshold. They administer the constraint and can influence its calibration through jurisprudence. They are the primary institutional actors capable of tuning the threshold.
narrative_ontology:constraint_stakeholder(supermajority_threshold__adaptive_gradient_reading, constitutional_courts, agenda_setter,
    institutional, generational, analytical, national).

% Bear the long-term reversibility costs of constitutional decisions made today. They have no voice in current threshold calibration and cannot exit the consequences of present constitutional rigidity or instability.
narrative_ontology:constraint_stakeholder(supermajority_threshold__adaptive_gradient_reading, future_generations, excluded,
    powerless, civilizational, trapped, national).

% Study supermajority thresholds across jurisdictions, measure consensus formation rates and reversibility costs, and provide evidence for calibration. They do not directly bear costs or collect benefits from any specific threshold.
narrative_ontology:constraint_stakeholder(supermajority_threshold__adaptive_gradient_reading, comparative_constitutional_scholars, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents constitutional instability from transient majorities while allowing adaptation when deep, persistent consensus exists. The threshold solves the collective-action problem of distinguishing fleeting preferences from enduring democratic will.
% TRANSFER_FUNCTION: Transfers veto power over constitutional change from simple majorities to supermajorities, effectively moving decision authority from current majorities to a broader consensus requirement that includes minority acquiescence.
% ABSENT_VOICES: Future generations who bear reversibility costs but cannot participate in current calibration debates; marginalized groups whose consensus formation rates may differ from the majority and are not captured in existing measurement frameworks.
% DISAPPEARANCE_RATIONALE: If the supermajority threshold vanished overnight, constitutional amendments would proceed by simple majority, likely increasing amendment frequency, reducing minority protections, and potentially destabilizing the constitutional order. The polity would reorganize around new amendment dynamics.
% FOUNDING_PROBLEM: Balancing constitutional stability against democratic responsiveness: preventing both frequent destabilizing changes driven by transient majorities and permanent entrenchment of outdated provisions that block necessary adaptation.
% FOUNDING_PROBLEM_CORROBORATION: Comparative constitutional law scholars (e.g., Richard Albert, Yaniv Roznai) and political scientists studying amendment difficulty (e.g., John Dinan) attest from outside the benefiting parties that the balancing problem persists and that empirical calibration of thresholds remains debated. No consensus exists on optimal thresholds.
narrative_ontology:disappearance_verdict(supermajority_threshold__adaptive_gradient_reading, world_rearranges).
narrative_ontology:founding_problem_status(supermajority_threshold__adaptive_gradient_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(supermajority_threshold__adaptive_gradient_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(supermajority_threshold__adaptive_gradient_reading, 'none', 1).
narrative_ontology:epsilon_provenance(supermajority_threshold__adaptive_gradient_reading, 0.35, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(supermajority_threshold__adaptive_gradient_reading_tests).
:- end_tests(supermajority_threshold__adaptive_gradient_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.35) reflects that current thresholds often exceed what consensus formation rates justify, creating moderate extraction from majorities. Suppression (0.45) is moderate because courts enforce the threshold but informal amendment pathways and judicial review provide partial alternatives. Theater (0.15) is low because the threshold performs real coordination (preventing instability) rather than mere performance. Accessibility collapse (0.65) is high because constitutional amendment rules are entrenched and alternatives (revolution, judicial reinterpretation) are costly. Resistance (0.5) is moderate: majorities push for lower thresholds, minorities defend higher ones, scholars advocate calibration. The claimed type 'rope' reflects the reading's view that a well-calibrated threshold is pure coordination; the metrics reflect the real-world miscalibration that introduces extractive elements.
 *
 * PERSPECTIVAL GAP:
 *   From the majority_factions seat, the threshold appears as a snare (high extraction, constrained exit). From the minority_factions seat, it appears as a rope (coordination benefit, low extraction). From the_polity seat, it is a rope when calibrated, tangled_rope when miscalibrated. From constitutional_courts, it is an agenda-setting tool they administer. The engine will compute these seat-specific classifications from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Majority_factions are payers (d near 1.0) because they bear the cost of supermajority requirements. Minority_factions are beneficiaries (d near 0.0) because they gain veto power. The_polity is a coordination beneficiary (d near 0.2) gaining stability. Constitutional_courts are agenda_setters (d near 0.3) with institutional power to shape calibration. Future_generations are excluded (d near 0.8) bearing irreversibility costs with no voice. Scholars are observers (d=0.5). Exit options differentiate: majorities have constrained exit (can push amendments but face high threshold), minorities have constrained exit (protected but locked in), polity has mobile exit (can migrate but not from constitutional order), courts have analytical exit (can reinterpret), future generations are trapped, scholars are analytical.
 *
 * MANDATROPHY ANALYSIS:
 *   The threshold's original mandate (balancing stability and responsiveness) remains live but contested. The adaptive gradient reading prevents mislabeling coordination as extraction by insisting on empirical calibration: if the threshold matches actual consensus formation rates, it is a rope; if it exceeds them, it becomes a snare. The mandate has not atrophied because the balancing problem persists, but the fixed threshold in many constitutions has become a piton (theatrical maintenance of an outdated calibration). The reading's evidence-based tuning requirement is the antidote to mandatrophy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    calibration_empirical_gap,
    'What are the actual social consensus formation rates and reversibility costs for constitutional changes in existing polities, and how do current supermajority thresholds compare?',
    'Comparative empirical study of amendment success rates, public opinion dynamics, and policy reversal costs across jurisdictions with varying thresholds.',
    'If current thresholds systematically exceed empirically justified levels, the adaptive gradient reading''s claim that they are miscalibrated snares is validated; if thresholds match empirical optima, the consensus_safeguard reading gains support.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(calibration_empirical_gap, empirical, 'Whether existing supermajority thresholds are empirically calibrated to consensus formation rates and reversibility costs.').

omega_variable(
    kernel_reading_structure,
    'How does the adaptive_gradient_reading structurally relate to the consensus_safeguard_reading and minoritarian_veto_reading of the supermajority_threshold kernel?',
    'Analyze whether the adaptive reading''s core premise (legitimacy requires empirical calibration) logically forecloses, coexists with, or influences the sibling readings'' premises (fixed deep consensus vs. entrenched minority veto).',
    'Determines the reading_relations in cs_structure and whether the kernel contains genuine foreclosure pairs or a coexisting triplet.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_structure, conceptual, 'Structural relationships among the three declared readings of the supermajority_threshold kernel.').

omega_variable(
    threshold_reversibility_measurement,
    'Can reversibility costs of constitutional decisions be measured in a way that is both politically legitimate and methodologically sound?',
    'Develop and test metrics for constitutional decision reversibility (e.g., amendment frequency, judicial reversal rates, policy feedback loops) across diverse constitutional systems.',
    'If reversibility costs are measurable, the adaptive gradient reading''s calibration framework becomes operational; if not, the reading''s empirical claim is undermined.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(threshold_reversibility_measurement, empirical, 'Feasibility of measuring constitutional reversibility costs for threshold calibration.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(supermajority_threshold__adaptive_gradient_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(supermajority_threshold__adaptive_gradient_reading_tr_t0, supermajority_threshold__adaptive_gradient_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement_basis(supermajority_threshold__adaptive_gradient_reading_tr_t0, observed).
narrative_ontology:measurement(supermajority_threshold__adaptive_gradient_reading_tr_t10, supermajority_threshold__adaptive_gradient_reading, theater_ratio, 10, 0.12).
narrative_ontology:measurement_basis(supermajority_threshold__adaptive_gradient_reading_tr_t10, observed).
narrative_ontology:measurement(supermajority_threshold__adaptive_gradient_reading_tr_t20, supermajority_threshold__adaptive_gradient_reading, theater_ratio, 20, 0.15).
narrative_ontology:measurement_basis(supermajority_threshold__adaptive_gradient_reading_tr_t20, observed).
narrative_ontology:measurement(supermajority_threshold__adaptive_gradient_reading_tr_t30, supermajority_threshold__adaptive_gradient_reading, theater_ratio, 30, 0.18).
narrative_ontology:measurement_basis(supermajority_threshold__adaptive_gradient_reading_tr_t30, observed).
narrative_ontology:measurement(supermajority_threshold__adaptive_gradient_reading_tr_t40, supermajority_threshold__adaptive_gradient_reading, theater_ratio, 40, 0.16).
narrative_ontology:measurement_basis(supermajority_threshold__adaptive_gradient_reading_tr_t40, observed).
narrative_ontology:measurement(supermajority_threshold__adaptive_gradient_reading_tr_t50, supermajority_threshold__adaptive_gradient_reading, theater_ratio, 50, 0.15).
narrative_ontology:measurement_basis(supermajority_threshold__adaptive_gradient_reading_tr_t50, observed).

% Extraction over time
narrative_ontology:measurement(supermajority_threshold__adaptive_gradient_reading_be_t0, supermajority_threshold__adaptive_gradient_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement_basis(supermajority_threshold__adaptive_gradient_reading_be_t0, observed).
narrative_ontology:measurement(supermajority_threshold__adaptive_gradient_reading_be_t10, supermajority_threshold__adaptive_gradient_reading, base_extractiveness, 10, 0.3).
narrative_ontology:measurement_basis(supermajority_threshold__adaptive_gradient_reading_be_t10, observed).
narrative_ontology:measurement(supermajority_threshold__adaptive_gradient_reading_be_t20, supermajority_threshold__adaptive_gradient_reading, base_extractiveness, 20, 0.35).
narrative_ontology:measurement_basis(supermajority_threshold__adaptive_gradient_reading_be_t20, observed).
narrative_ontology:measurement(supermajority_threshold__adaptive_gradient_reading_be_t30, supermajority_threshold__adaptive_gradient_reading, base_extractiveness, 30, 0.4).
narrative_ontology:measurement_basis(supermajority_threshold__adaptive_gradient_reading_be_t30, observed).
narrative_ontology:measurement(supermajority_threshold__adaptive_gradient_reading_be_t40, supermajority_threshold__adaptive_gradient_reading, base_extractiveness, 40, 0.38).
narrative_ontology:measurement_basis(supermajority_threshold__adaptive_gradient_reading_be_t40, observed).
narrative_ontology:measurement(supermajority_threshold__adaptive_gradient_reading_be_t50, supermajority_threshold__adaptive_gradient_reading, base_extractiveness, 50, 0.35).
narrative_ontology:measurement_basis(supermajority_threshold__adaptive_gradient_reading_be_t50, observed).

% Suppression requirement over time
narrative_ontology:measurement(supermajority_threshold__adaptive_gradient_reading_su_t0, supermajority_threshold__adaptive_gradient_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement_basis(supermajority_threshold__adaptive_gradient_reading_su_t0, observed).
narrative_ontology:measurement(supermajority_threshold__adaptive_gradient_reading_su_t10, supermajority_threshold__adaptive_gradient_reading, suppression_requirement, 10, 0.42).
narrative_ontology:measurement_basis(supermajority_threshold__adaptive_gradient_reading_su_t10, observed).
narrative_ontology:measurement(supermajority_threshold__adaptive_gradient_reading_su_t20, supermajority_threshold__adaptive_gradient_reading, suppression_requirement, 20, 0.45).
narrative_ontology:measurement_basis(supermajority_threshold__adaptive_gradient_reading_su_t20, observed).
narrative_ontology:measurement(supermajority_threshold__adaptive_gradient_reading_su_t30, supermajority_threshold__adaptive_gradient_reading, suppression_requirement, 30, 0.48).
narrative_ontology:measurement_basis(supermajority_threshold__adaptive_gradient_reading_su_t30, observed).
narrative_ontology:measurement(supermajority_threshold__adaptive_gradient_reading_su_t40, supermajority_threshold__adaptive_gradient_reading, suppression_requirement, 40, 0.46).
narrative_ontology:measurement_basis(supermajority_threshold__adaptive_gradient_reading_su_t40, observed).
narrative_ontology:measurement(supermajority_threshold__adaptive_gradient_reading_su_t50, supermajority_threshold__adaptive_gradient_reading, suppression_requirement, 50, 0.45).
narrative_ontology:measurement_basis(supermajority_threshold__adaptive_gradient_reading_su_t50, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(supermajority_threshold__adaptive_gradient_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(supermajority_threshold__adaptive_gradient_reading, 0.12).
narrative_ontology:affects_constraint(supermajority_threshold__adaptive_gradient_reading, constitutional_amendment_process).
narrative_ontology:affects_constraint(supermajority_threshold__adaptive_gradient_reading, judicial_review_standards).
narrative_ontology:affects_constraint(supermajority_threshold__adaptive_gradient_reading, federalism_power_allocation).

% DUAL FORMULATION NOTE:
% This adaptive_gradient_reading decomposes the supermajority_threshold kernel with consensus_safeguard_reading and minoritarian_veto_reading. The adaptive reading treats the threshold as a tunable parameter (ε varies with calibration), while the sibling readings treat it as a fixed principle (consensus safeguard) or a power resource (minoritarian veto). The ε-invariance principle requires separate stories because the referent (the threshold) is evaluated under different calibration assumptions, yielding different extraction profiles.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(supermajority_threshold__adaptive_gradient_reading, institutional, 0.3).
constraint_indexing:directionality_override(supermajority_threshold__adaptive_gradient_reading, powerless, 0.8).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
