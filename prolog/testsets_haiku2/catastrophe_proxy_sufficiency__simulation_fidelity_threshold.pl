% ============================================================================
% CONSTRAINT STORY: catastrophe_proxy_sufficiency__simulation_fidelity_threshold
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_proxy_sufficiency__simulation_fidelity_threshold, []).

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
 *   constraint_id: catastrophe_proxy_sufficiency__simulation_fidelity_threshold
 *   human_readable: Simulation Fidelity Threshold for Competence Retention
 *   domain: safety_engineering/organizational_learning
 *
 * SUMMARY:
 *   This is one reading of a contested kernel: the question of whether
 *   simulation can maintain operational competence in high-reliability
 *   systems without real catastrophic events. The fidelity-threshold reading
 *   asserts that competence retention depends on simulation crossing a
 *   measurable threshold where stress/uncertainty matches real catastrophe —
 *   sufficiency is technology-dependent and calibrable, not categorical. This
 *   reading benefits simulation technology vendors, who gain ongoing revenue
 *   from fidelity improvement, and is enforced by regulatory authorities who
 *   codify threshold standards. It extracts from high-reliability
 *   organizations (capital cost of ever-better systems) and operational
 *   personnel (psychological cost of training-only competence). The
 *   constraint is authored as rope (coordination via technology standards)
 *   under this reading, though the divergent sibling readings would classify
 *   it differently.
 *
 * KEY AGENTS:
 *   - simulation_technology_vendors: Primary beneficiary; shapes fidelity standards; powerful institutional actor with global reach
 *   - high_reliability_organizations: Primary payer; absorbed into technology upgrade cycle; constrained exit (unethical to refuse simulation)
 *   - operational_personnel: Payer and identity-locked; carries uncertainty of whether simulation stress calibrates to real stress; professional identity fused to competence claim
 *   - catastrophe_risk_bearers: Trapped, powerless; exposed to tail risk if simulation proves insufficient; excluded from threshold-setting
 *   - regulatory_authorities: Agenda-setter; codifies thresholds into law; pressured by vendors and safety advocates
 *   - epistemology_researchers: Observer; produces evidence on simulation sufficiency; findings feed consensus but lack enforcement power
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, 0.68).
domain_priors:suppression_score(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, 0.61).
domain_priors:theater_ratio(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, extractiveness, 0.68).
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, suppression_requirement, 0.61).
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, rope).
narrative_ontology:human_readable(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, "Simulation Fidelity Threshold for Competence Retention").
narrative_ontology:topic_domain(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, "safety_engineering/organizational_learning").

domain_priors:requires_active_enforcement(catastrophe_proxy_sufficiency__simulation_fidelity_threshold).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, 'dd58c393-19cb-4b10-a34d-58da9b325cde').
narrative_ontology:cs_kernel_codification('dd58c393-19cb-4b10-a34d-58da9b325cde', fixed_text).
narrative_ontology:cs_authority_grounding('dd58c393-19cb-4b10-a34d-58da9b325cde', extraction).
narrative_ontology:cs_interpretation_layer_present('dd58c393-19cb-4b10-a34d-58da9b325cde').
narrative_ontology:cs_reading_relation('dd58c393-19cb-4b10-a34d-58da9b325cde', catastrophe_proxy_sufficiency__catastrophe_necessity_reading, forecloses).
narrative_ontology:cs_reading_relation('dd58c393-19cb-4b10-a34d-58da9b325cde', catastrophe_proxy_sufficiency__hybrid_degradation_reading, influences).
narrative_ontology:cs_reading_relation('dd58c393-19cb-4b10-a34d-58da9b325cde', catastrophe_proxy_sufficiency__simulation_as_proxy_catastrophe_reading, coexists_with).
narrative_ontology:cs_axiom('dd58c393-19cb-4b10-a34d-58da9b325cde', foundational, fidelity_calibration_sufficient).
narrative_ontology:cs_axiom_status(fidelity_calibration_sufficient, holdable).
narrative_ontology:cs_axiom_grounding('dd58c393-19cb-4b10-a34d-58da9b325cde', fidelity_calibration_sufficient, empirically_contingent).
narrative_ontology:cs_axiom('dd58c393-19cb-4b10-a34d-58da9b325cde', secondary, technology_investment_validates_threshold).
narrative_ontology:cs_axiom_status(technology_investment_validates_threshold, holdable).
narrative_ontology:cs_axiom_grounding('dd58c393-19cb-4b10-a34d-58da9b325cde', technology_investment_validates_threshold, conventional).
narrative_ontology:cs_reference_frame('dd58c393-19cb-4b10-a34d-58da9b325cde', simulation_sufficiency_via_fidelity_calibration).
narrative_ontology:cs_drift_state('dd58c393-19cb-4b10-a34d-58da9b325cde', contemporary_tacit_knowledge_loss_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('dd58c393-19cb-4b10-a34d-58da9b325cde', '').
narrative_ontology:cs_kernel_id(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, catastrophe_proxy_sufficiency).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, simulation_technology_vendors).
narrative_ontology:constraint_beneficiary(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, training_program_operators).
narrative_ontology:constraint_victim(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, high_reliability_organizations).
narrative_ontology:constraint_victim(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, operational_personnel).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, catastrophe_risk_bearers).
narrative_ontology:constraint_vindicates(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, technology_sufficiency_hypothesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Manufactures and sells simulation platforms that organizations adopt to maintain operational competence without real-world catastrophes. Benefits from the fidelity-threshold framing because it justifies continuous investment in higher-fidelity systems, sensor arrays, and scenario libraries. Sets the standards by which fidelity sufficiency is measured, influencing what counts as 'good enough' simulation.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, simulation_technology_vendors, beneficiary,
    powerful, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, simulation_technology_vendors, agenda_setter).

% Nuclear plants, aviation operations, surgical teams, emergency response systems. Must demonstrate competence retention without regular real catastrophes (ethical/legal prohibition). They bear the capital and operational cost of simulation systems and the organizational burden of continuous technology upgrade cycles. Their exit option is accepting degradation risk or depending on rare catastrophic events for competence refresh — both unacceptable.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, high_reliability_organizations, payer,
    institutional, generational, constrained, global).

% Pilots, nuclear reactor operators, surgeons, first responders whose competence is maintained through simulation. They absorb the time cost of training, the psychological cost of never knowing whether simulation stress truly calibrates to real stress, and career risk if simulation-only training proves insufficient. Their professional identity is constituted through the competence claim; leaving the profession means rejecting the identity entirely.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, operational_personnel, payer,
    moderate, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, operational_personnel, excluded).

% Passengers, patients, nearby communities who depend on the competence of operational personnel. They bear the tail risk if simulation-only training proves insufficient at the moment of real catastrophe. They have no voice in what simulation fidelity is considered 'good enough' and cannot exit the exposure.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, catastrophe_risk_bearers, payer,
    powerless, immediate, trapped, local).

% Cognitive scientists, human factors specialists, complexity theorists who study whether simulation can truly substitute for real-world stress and uncertainty. They produce evidence on the question but lack enforcement authority; their findings feed into the contested claim without settling it.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, epistemology_researchers, observer,
    organized, generational, analytical, global).

% Nuclear regulators, aviation authorities, medical boards. They codify simulation fidelity standards into regulatory requirements, enforcing organizations to adopt approved systems. They face pressure from both vendors (who lobby for generous fidelity thresholds) and safety advocates (who argue thresholds are insufficient). Their enforcement power creates the structural hold on the constraint.
narrative_ontology:constraint_stakeholder(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, regulatory_authorities, agenda_setter,
    institutional, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, simulation_technology_vendors).
narrative_ontology:fixing_cost_class(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables high-reliability organizations to maintain operational competence in the absence of real catastrophic events, which are ethically prohibited and statistically rare. Solves the coordination problem of how to generate the stress/uncertainty necessary for genuine competence refresh without the catastrophe itself.
% TRANSFER_FUNCTION: Capital and operational resources flow from high-reliability organizations to simulation technology vendors. Time and psychological burden flow from operational personnel into continuous training cycles. Regulatory approval flows from authorities to vendors whose systems meet declared fidelity thresholds, creating a standards-setter role that vendors actively shape.
% ABSENT_VOICES: Catastrophe risk bearers (patients, passengers, communities) are structurally excluded from the threshold-setting conversation. They would argue for maximum conservatism ('if simulation cannot prove equivalence to real catastrophe, assume it is insufficient'); they are not consulted in standard-setting. Competing epistemologies (those asserting only real catastrophes maintain competence) are marginalized by the threshold framing itself.
% DISAPPEARANCE_RATIONALE: If the fidelity-threshold constraint disappeared, organizations would either return to real-catastrophe-based competence refresh (unethical/illegal), accept unvalidated degradation risk, or cease operations. The constraint's disappearance would force a fundamental reorganization of how high-reliability organizations maintain competence.
% FOUNDING_PROBLEM: How to maintain operational competence in high-reliability systems when catastrophic events are ethically prohibited, legally restricted, or statistically rare — and simulation has become technologically available as a potential substitute.
% FOUNDING_PROBLEM_CORROBORATION: Nuclear regulators, aviation authorities, and medical boards all formally attest to this problem. Independent human factors research (Dekker, Woods, Hollnagel) documents the competence-maintenance gap. The vendor community's technical claims provide self-interested attestation. The constraint was formally instantiated in regulatory frameworks in the 1970s–1990s across multiple domains, documented in regulatory history and agency records from sources independent of current vendors.
narrative_ontology:disappearance_verdict(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_proxy_sufficiency__simulation_fidelity_threshold_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(catastrophe_proxy_sufficiency__simulation_fidelity_threshold_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness measures at 0.68 because the constraint creates an ongoing need for technology investment without providing verifiable proof that the investment solves the foundational problem. Vendors benefit from the ambiguity: the more the competence maintenance gap persists in public perception, the more organizations justify higher technology investment. Suppression is high (0.61) because regulatory authorities enforce the fidelity threshold against competing epistemic claims (catastrophe_necessity_reading, hybrid_degradation_reading) that would lower technology investment or demand different validation. Theater ratio is moderate-high (0.42) because a growing share of simulation training activity is devoted to compliance demonstration (meeting regulatory fidelity marks) rather than actual competence building. Accessibility_collapse is moderate (0.58) because alternative framings remain intellectually live — the catastrophe_necessity reading has scholarly support — but regulatory codification makes deviation from simulation-based training institutionally difficult. Resistance is high (0.72) because operational personnel harbor substantial doubt about whether simulation stress truly calibrates to real stress, and epistemology researchers continue to publish findings about tacit knowledge loss and stress-response gaps. The measurements show extractiveness rising steeply from 0.42 to 0.68 over the first 30 time points (roughly 1970–2000, reflecting the transition from real-catastrophe-based training to simulation-only regimes) and then plateauing, suggesting the constraint has reached a steady state: the ambiguity about simulation sufficiency is now foundational, not transitional.
 *
 * PERSPECTIVAL GAP:
 *   From the vendor and regulatory perspective, the constraint is genuine rope: solving a real coordination problem (how to maintain competence without catastrophes) through technology standards that all parties agree to adopt. The coordination benefit is real — better to have standards than chaotic fragmentation. From the high-reliability organization perspective, the constraint is extractive rope: they are locked into a technology upgrade cycle that may not improve competence, with vendors capturing the revenue and regulators enforcing the standards. From operational personnel's perspective, the constraint is identity-locking snare: they cannot exit without rejecting their professional identity, and they absorb psychological uncertainty about whether their simulation-trained competence will transfer to real catastrophe. The engine computes these divergent types from the structural data: vendor (beneficiary, powerful, arbitrage exit) vs. organization (payer, institutional, constrained exit) vs. personnel (payer, moderate, identity_locked exit) will show distinct directionality vectors and per-seat type assignments.
 *
 * DIRECTIONALITY LOGIC:
 *   Simulation technology vendors sit at d ≈ 0.0–0.15 (beneficiary end): they collect from the constraint, can exit to other markets if this one fails, and actively shape the standards that determine sufficiency. High-reliability organizations sit at d ≈ 0.6–0.75 (payer end): they bear capital costs, have constrained exit (cannot ethically refuse competence maintenance), and do not control the fidelity standards. Operational personnel sit at d ≈ 0.7–0.85 (target end): they absorb time and psychological cost, are identity-locked (cannot exit without rejecting professional identity), and lack voice in threshold-setting. Catastrophe risk bearers sit at d ≈ 0.95 (full target): they bear tail risk, are trapped (cannot exit exposure), and are excluded from the conversation entirely. Regulatory authorities sit at d ≈ 0.4–0.5 (near-symmetric): they benefit from having a standard (reduces liability exposure, enables consistent governance), but also bear reputational cost if simulation-only training proves insufficient and a real catastrophe reveals competence gaps.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (maintaining competence without catastrophes) was live and urgent in the 1970s–1980s when simulation technology emerged as a credible substitute. Today (status: live, but contested) the problem is whether simulation has actually solved it or merely displaced the incompetence onto a different dimension (tacit knowledge loss, stress-response degradation, generational decay). The constraint persists not because the founding problem is solved, but because (a) the alternative (accepting unvalidated competence gaps) is unacceptable, (b) the constraint's beneficiaries (vendors, regulatory authorities) have institutional incentive to treat it as solved, and (c) operational personnel cannot easily defect. The mandatrophy is partial: the constraint retains enough coordination function (preventing pure fragmentation) to persist, but the competence-maintenance function is increasingly contested. This reading (fidelity-threshold) embeds the ambiguity: it asserts competence CAN be maintained if simulation reaches sufficient fidelity, but leaves the sufficiency question unanswered — that is precisely the gap vendors and regulators exploit to justify continued investment and enforcement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fidelity_sufficiency_empirical_gap,
    'Can simulation-induced stress and uncertainty ever achieve provable equivalence to real-catastrophe stress, or is the gap irreducible by epistemic design?',
    'Empirical study: compare stress biomarkers (cortisol, heart rate variability, decision latency) in simulation-trained personnel during real-catastrophe events vs. simulation-naive personnel. Track performance outcomes and error rates at moment of crisis. Longitudinal cognitive follow-up to detect tacit knowledge loss in simulation-only cohorts.',
    'If equivalence is provable and measurable, the fidelity-threshold framing is epistemically sound. If the gap is irreducible, the constraint is technologically over-confident and vendors'' beneficiary position is unsustainable. If provably unmeasurable, the threshold remains a policy choice, not a discovered fact.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fidelity_sufficiency_empirical_gap, empirical, 'Whether simulation fidelity can achieve epistemic equivalence to real catastrophe stress.').

omega_variable(
    vendor_capture_of_standards,
    'To what degree do simulation technology vendors shape the regulatory standards used to certify their own products as ''sufficiently fidelitous''?',
    'Regulatory process audit: trace the origin and revision history of fidelity standards; document vendor participation in standard-setting committees; compare standards that emerged from vendor-led processes vs. independent technical bodies; measure correlation between standard tightness and vendor R&D capability.',
    'High vendor capture would indicate the fidelity threshold is optimized for vendor benefit rather than competence maintenance — the constraint would shift from rope to tangled_rope or snare. Low capture would support the rope framing.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(vendor_capture_of_standards, empirical, 'Regulatory capture dynamics in simulation fidelity standard-setting.').

omega_variable(
    tacit_knowledge_generational_loss,
    'Does simulation-only training, applied across multiple generational cohorts of personnel, produce cumulative loss of tacit knowledge (intuitive pattern recognition, stress-response calibration, embodied expertise) that would only be recovered through real-catastrophe exposure?',
    'Longitudinal anthropological/cognitive study of high-reliability organizations over 20+ years: compare tacit knowledge depth (measured via expert elicitation, decision trace analysis, near-miss recovery) in organizations that had real-catastrophe events vs. simulation-only organizations. Track whether tacit knowledge loss accumulates across generational handoff.',
    'Substantial tacit loss across generations would support the catastrophe_necessity_reading and undermine this reading''s sufficiency claim. Loss limited to individual-career timescales would support hybrid_degradation_reading. No measurable loss would vindicate this reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(tacit_knowledge_generational_loss, empirical, 'Whether simulation-only training produces irreversible generational loss of tacit competence.').

omega_variable(
    fidelity_threshold_vs_reading_frame,
    'Is the ''fidelity threshold'' framing a genuine epistemic discovery (simulation can be calibrated to equivalence) or a committer choice (we commit to treating simulation as sufficient if it meets certain technological marks, regardless of theoretical equivalence)?',
    'Philosophical/methodological analysis: examine regulatory and vendor discourse to determine whether the threshold is asserted as discovered fact or policy choice. Cross-read with foundational HCI/human factors literature on transfer-of-training; distinguish transfer-proven conditions from transfer-assumed conditions.',
    'If the threshold is committer-chosen (policy), the constraint is rope-on-convention, not rope-on-coordination: the beneficiary (vendors) and agenda-setter (regulators) chose the threshold jointly, and alternative thresholds remain live. If discovered, the framing is more robust but vendors cannot shift the standard unilaterally. This frames the reading vs. its siblings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(fidelity_threshold_vs_reading_frame, conceptual, 'Whether fidelity threshold is epistemic discovery or committer policy choice.').

omega_variable(
    sibling_reading_framing_under_determination,
    'This reading instantiates the threshold as technology-dependent sufficiency. The catastrophe_necessity_reading asserts only real catastrophes maintain competence. The hybrid_degradation_reading asserts simulation maintains procedural knowledge but not stress-response capacity. The simulation_as_proxy_catastrophe_reading asserts simulation is indefinitely sufficient. How much of the disagreement is empirical (about what simulation can/cannot do) vs. axiological (about what risk level is acceptable)?',
    'Decompose each reading''s core premise into empirical and axiological components. Empirical: What does simulation provably accomplish? What does it provably fail to accomplish? Axiological: Given those bounds, what level of residual incompetence is acceptable? At what tail-risk threshold does a rational actor accept simulation-only training? The readings may agree on empirical bounds but differ on acceptable risk — or they may dispute the empirical bounds themselves. Clarify which.',
    'High axiological divergence with empirical agreement would indicate this reading is a policy preference, not a technical discovery. High empirical divergence would indicate the readings describe different domains (e.g., one applies to procedural tasks, another to crisis response). This affects whether the sibling readings truly foreclose each other or merely prefer different risk thresholds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_framing_under_determination, conceptual, 'Decomposition of empirical vs. axiological divergence among sibling readings of the catastrophe_proxy_sufficiency kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(catastrophe_proxy_sim_fidelity_tr_t0, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(catastrophe_proxy_sim_fidelity_tr_t0, observed).
narrative_ontology:measurement(catastrophe_proxy_sim_fidelity_tr_t5, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, theater_ratio, 5, 0.22).
narrative_ontology:measurement_basis(catastrophe_proxy_sim_fidelity_tr_t5, observed).
narrative_ontology:measurement(catastrophe_proxy_sim_fidelity_tr_t10, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, theater_ratio, 10, 0.27).
narrative_ontology:measurement_basis(catastrophe_proxy_sim_fidelity_tr_t10, observed).
narrative_ontology:measurement(catastrophe_proxy_sim_fidelity_tr_t15, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, theater_ratio, 15, 0.32).
narrative_ontology:measurement_basis(catastrophe_proxy_sim_fidelity_tr_t15, observed).
narrative_ontology:measurement(catastrophe_proxy_sim_fidelity_tr_t20, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, theater_ratio, 20, 0.37).
narrative_ontology:measurement_basis(catastrophe_proxy_sim_fidelity_tr_t20, observed).
narrative_ontology:measurement(catastrophe_proxy_sim_fidelity_tr_t25, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, theater_ratio, 25, 0.4).
narrative_ontology:measurement_basis(catastrophe_proxy_sim_fidelity_tr_t25, observed).
narrative_ontology:measurement(catastrophe_proxy_sim_fidelity_tr_t30, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, theater_ratio, 30, 0.42).
narrative_ontology:measurement_basis(catastrophe_proxy_sim_fidelity_tr_t30, observed).
narrative_ontology:measurement(catastrophe_proxy_sim_fidelity_tr_t40, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, theater_ratio, 40, 0.42).
narrative_ontology:measurement_basis(catastrophe_proxy_sim_fidelity_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(catastrophe_proxy_sim_fidelity_be_t0, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, base_extractiveness, 0, 0.42).
narrative_ontology:measurement_basis(catastrophe_proxy_sim_fidelity_be_t0, observed).
narrative_ontology:measurement(catastrophe_proxy_sim_fidelity_be_t5, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, base_extractiveness, 5, 0.48).
narrative_ontology:measurement_basis(catastrophe_proxy_sim_fidelity_be_t5, observed).
narrative_ontology:measurement(catastrophe_proxy_sim_fidelity_be_t10, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, base_extractiveness, 10, 0.54).
narrative_ontology:measurement_basis(catastrophe_proxy_sim_fidelity_be_t10, observed).
narrative_ontology:measurement(catastrophe_proxy_sim_fidelity_be_t15, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, base_extractiveness, 15, 0.59).
narrative_ontology:measurement_basis(catastrophe_proxy_sim_fidelity_be_t15, observed).
narrative_ontology:measurement(catastrophe_proxy_sim_fidelity_be_t20, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, base_extractiveness, 20, 0.64).
narrative_ontology:measurement_basis(catastrophe_proxy_sim_fidelity_be_t20, observed).
narrative_ontology:measurement(catastrophe_proxy_sim_fidelity_be_t25, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, base_extractiveness, 25, 0.67).
narrative_ontology:measurement_basis(catastrophe_proxy_sim_fidelity_be_t25, observed).
narrative_ontology:measurement(catastrophe_proxy_sim_fidelity_be_t30, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, base_extractiveness, 30, 0.68).
narrative_ontology:measurement_basis(catastrophe_proxy_sim_fidelity_be_t30, observed).
narrative_ontology:measurement(catastrophe_proxy_sim_fidelity_be_t40, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, base_extractiveness, 40, 0.68).
narrative_ontology:measurement_basis(catastrophe_proxy_sim_fidelity_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(catastrophe_proxy_sim_fidelity_su_t0, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, suppression_requirement, 0, 0.48).
narrative_ontology:measurement_basis(catastrophe_proxy_sim_fidelity_su_t0, observed).
narrative_ontology:measurement(catastrophe_proxy_sim_fidelity_su_t5, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, suppression_requirement, 5, 0.52).
narrative_ontology:measurement_basis(catastrophe_proxy_sim_fidelity_su_t5, observed).
narrative_ontology:measurement(catastrophe_proxy_sim_fidelity_su_t10, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, suppression_requirement, 10, 0.56).
narrative_ontology:measurement_basis(catastrophe_proxy_sim_fidelity_su_t10, observed).
narrative_ontology:measurement(catastrophe_proxy_sim_fidelity_su_t15, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, suppression_requirement, 15, 0.58).
narrative_ontology:measurement_basis(catastrophe_proxy_sim_fidelity_su_t15, observed).
narrative_ontology:measurement(catastrophe_proxy_sim_fidelity_su_t20, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, suppression_requirement, 20, 0.6).
narrative_ontology:measurement_basis(catastrophe_proxy_sim_fidelity_su_t20, observed).
narrative_ontology:measurement(catastrophe_proxy_sim_fidelity_su_t25, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, suppression_requirement, 25, 0.61).
narrative_ontology:measurement_basis(catastrophe_proxy_sim_fidelity_su_t25, observed).
narrative_ontology:measurement(catastrophe_proxy_sim_fidelity_su_t30, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, suppression_requirement, 30, 0.61).
narrative_ontology:measurement_basis(catastrophe_proxy_sim_fidelity_su_t30, observed).
narrative_ontology:measurement(catastrophe_proxy_sim_fidelity_su_t40, catastrophe_proxy_sufficiency__simulation_fidelity_threshold, suppression_requirement, 40, 0.61).
narrative_ontology:measurement_basis(catastrophe_proxy_sim_fidelity_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, resource_allocation).
narrative_ontology:boltzmann_floor_override(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, 0.12).
narrative_ontology:affects_constraint(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, catastrophe_necessity_reading).
narrative_ontology:affects_constraint(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, hybrid_degradation_reading).
narrative_ontology:affects_constraint(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, simulation_as_proxy_catastrophe_reading).

% DUAL FORMULATION NOTE:
% This constraint family decomposes the catastrophe_proxy_sufficiency kernel into four structurally distinct constraints, each authored as a different reading of the same contested commitment (can simulation maintain competence without real catastrophes?). The family captures a genuine kernel dispute: parties committed to maintaining competence without catastrophes disagree fundamentally on what simulation can epistically accomplish. Each reading produces different ε values, beneficiary structures, and type assignments. This story instantiates the fidelity-threshold reading; the sibling stories instantiate the alternatives. Network links capture the epistemic influence: claims about simulation sufficiency at high fidelity (this reading) upstream-influence claims about whether simulation ever suffices (proxy_catastrophe reading) and whether incomplete simulation drives generational degradation (hybrid_degradation reading). The catastrophe_necessity_reading forecloses all three by denying that simulation can ever be sufficient.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(catastrophe_proxy_sufficiency__simulation_fidelity_threshold, institutional, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
