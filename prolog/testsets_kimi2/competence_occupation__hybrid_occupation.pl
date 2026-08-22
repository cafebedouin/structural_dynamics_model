% ============================================================================
% CONSTRAINT STORY: competence_occupation__hybrid_occupation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-01-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_competence_occupation__hybrid_occupation, []).

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
 *   constraint_id: competence_occupation__hybrid_occupation
 *   human_readable: Hybrid Multi-Mechanism Competence Occupation
 *   domain: high_reliability_organizations/safety_training
 *
 * SUMMARY:
 *   This constraint instantiates the hybrid_occupation reading of the
 *   competence_occupation kernel in high-reliability organizations. It posits
 *   that competence must be continuously occupied through multiple
 *   simultaneous mechanismsâsimulation, refresher training, procedural
 *   reinforcement, and live line auditsâand that no single mechanism or
 *   fixed configuration is sufficient. The arrangement coordinates genuine
 *   catastrophic-risk reduction while extracting perpetual resources from
 *   operators and organizations, with training-system vendors and external
 *   auditors as concentrated beneficiaries. The sibling readings
 *   (simulation_sufficiency, real_incident_necessity) are structurally
 *   distinct and are modeled as separate constraints in the same family.
 *
 * KEY AGENTS:
 *   - frontline_operators (moderate/constrained): Primary target â bear cognitive load, time extraction, and skill-fade anxiety.
 *   - operating_organizations (institutional/constrained): Dual-positioned payer â fund the hybrid infrastructure and bear compliance costs while receiving liability reduction.
 *   - training_system_vendors (organized/mobile): Primary beneficiary â capture revenue from perpetual procurement driven by open-ended training mandates.
 *   - external_safety_auditors (organized/mobile): Secondary beneficiary â professional authority and income depend on perpetual external validation.
 *   - regulatory_authorities (institutional/analytical): Agenda setter â mandates multi-mechanism regimes; vulnerability to expertise capture.
 *   - streamlined_training_advocates (moderate/constrained): Excluded voice â argues for simpler regimes but lacks standing in standard-setting.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(competence_occupation__hybrid_occupation, 0.62).
domain_priors:suppression_score(competence_occupation__hybrid_occupation, 0.58).
domain_priors:theater_ratio(competence_occupation__hybrid_occupation, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(competence_occupation__hybrid_occupation, extractiveness, 0.62).
narrative_ontology:constraint_metric(competence_occupation__hybrid_occupation, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(competence_occupation__hybrid_occupation, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(competence_occupation__hybrid_occupation, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(competence_occupation__hybrid_occupation, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(competence_occupation__hybrid_occupation, tangled_rope).
narrative_ontology:human_readable(competence_occupation__hybrid_occupation, "Hybrid Multi-Mechanism Competence Occupation").
narrative_ontology:topic_domain(competence_occupation__hybrid_occupation, "high_reliability_organizations/safety_training").

domain_priors:requires_active_enforcement(competence_occupation__hybrid_occupation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(competence_occupation__hybrid_occupation, '93f2b741-ce24-488d-836e-43995c520f6f').
narrative_ontology:cs_kernel_codification('93f2b741-ce24-488d-836e-43995c520f6f', distributed).
narrative_ontology:cs_authority_grounding('93f2b741-ce24-488d-836e-43995c520f6f', expertise).
narrative_ontology:cs_interpretation_layer_present('93f2b741-ce24-488d-836e-43995c520f6f').
narrative_ontology:cs_reading_relation('93f2b741-ce24-488d-836e-43995c520f6f', competence_occupation__simulation_sufficiency, coexists_with).
narrative_ontology:cs_reading_relation('93f2b741-ce24-488d-836e-43995c520f6f', competence_occupation__real_incident_necessity, coexists_with).
narrative_ontology:cs_axiom('93f2b741-ce24-488d-836e-43995c520f6f', foundational, no_single_mechanism_sufficient).
narrative_ontology:cs_axiom_status(no_single_mechanism_sufficient, holdable).
narrative_ontology:cs_axiom_grounding('93f2b741-ce24-488d-836e-43995c520f6f', no_single_mechanism_sufficient, empirically_contingent).
narrative_ontology:cs_axiom('93f2b741-ce24-488d-836e-43995c520f6f', foundational, continuous_exercise_mandatory).
narrative_ontology:cs_axiom_status(continuous_exercise_mandatory, holdable).
narrative_ontology:cs_axiom_grounding('93f2b741-ce24-488d-836e-43995c520f6f', continuous_exercise_mandatory, instrumental).
narrative_ontology:cs_reference_frame('93f2b741-ce24-488d-836e-43995c520f6f', multi_modal_competence_maintenance).
narrative_ontology:cs_drift_state('93f2b741-ce24-488d-836e-43995c520f6f', perpetual_research_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('93f2b741-ce24-488d-836e-43995c520f6f', '').
narrative_ontology:cs_kernel_id(competence_occupation__hybrid_occupation, competence_occupation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(competence_occupation__hybrid_occupation, training_system_vendors).
narrative_ontology:constraint_beneficiary(competence_occupation__hybrid_occupation, external_safety_auditors).
narrative_ontology:constraint_victim(competence_occupation__hybrid_occupation, frontline_operators).
narrative_ontology:constraint_victim(competence_occupation__hybrid_occupation, operating_organizations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(competence_occupation__hybrid_occupation, operating_organizations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Must recurrently demonstrate competence through simulation, refresher drills, procedural reinforcement, and live line audits. The absence of consensus on optimal configuration means training loads expand unpredictably. Exit requires leaving the profession or losing certification.
narrative_ontology:constraint_stakeholder(competence_occupation__hybrid_occupation, frontline_operators, payer,
    moderate, biographical, constrained, national).

% Fund and administer perpetual multi-modal training infrastructure. Bear escalating compliance costs while receiving liability reduction and genuine safety improvements. Cannot opt out of the regulatory mandate.
narrative_ontology:constraint_stakeholder(competence_occupation__hybrid_occupation, operating_organizations, payer,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(competence_occupation__hybrid_occupation, operating_organizations, beneficiary).

% Sell and maintain simulation systems, drill packages, and audit tools. Benefit from regulatory mandates requiring continuous multi-mechanism exercise and from the absence of consensus that would otherwise standardize and commoditize training.
narrative_ontology:constraint_stakeholder(competence_occupation__hybrid_occupation, training_system_vendors, beneficiary,
    organized, biographical, mobile, global).

% Conduct line audits and competence verification. Their professional authority and revenue depend on the perpetual need for external validation of hybrid training outcomes.
narrative_ontology:constraint_stakeholder(competence_occupation__hybrid_occupation, external_safety_auditors, beneficiary,
    organized, biographical, mobile, national).

% Mandate recurrent training and multi-mechanism competence verification. Set evolving standards justified by catastrophic-risk prevention. Authority is vulnerable to capture by training-industry expertise.
narrative_ontology:constraint_stakeholder(competence_occupation__hybrid_occupation, regulatory_authorities, agenda_setter,
    institutional, generational, analytical, national).

% Researchers and practitioners who argue that specific competence domains might be maintained with fewer mechanisms or lower frequency. Excluded from standard-setting bodies dominated by safety-auditor expertise and regulatory caution.
narrative_ontology:constraint_stakeholder(competence_occupation__hybrid_occupation, streamlined_training_advocates, excluded,
    moderate, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(competence_occupation__hybrid_occupation, diffuse).
narrative_ontology:fixing_cost_class(competence_occupation__hybrid_occupation, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains operator competence across multiple skill domains in high-consequence environments where skill decay is inevitable and catastrophic failure is possible; coordinates the distribution of practice across simulation, procedural drill, and live audit to cover gaps no single mechanism can fill.
% TRANSFER_FUNCTION: Moves time, attention, and organizational budget from frontline operators and operating organizations to training system vendors, auditor firms, and regulatory compliance infrastructure.
% ABSENT_VOICES: Streamlined training advocates and alternative-pedagogy researchers who argue that specific competence domains might be maintained with fewer or different mechanisms, but are excluded from standard-setting because the hybrid model is the regulatory default.
% DISAPPEARANCE_RATIONALE: If the multi-mechanism requirement vanished, organizations would revert to single-mode training (cheapest option), vendors and auditors would lose a core revenue stream, and the current safety assurance architecture would collapse into ad-hoc practice.
% FOUNDING_PROBLEM: Catastrophic accidents in high-reliability domains revealed that single-mode training left critical competence gaps unoccupied; the hybrid model was constructed to cover more failure modes.
% FOUNDING_PROBLEM_CORROBORATION: Accident investigation boards (independent of training vendors) attest that competence gaps contributed to historical incidents; however, efficiency researchers outside the beneficiary set contest that the current multi-mechanism overload is proportionate to the founding risk.
narrative_ontology:disappearance_verdict(competence_occupation__hybrid_occupation, world_rearranges).
narrative_ontology:founding_problem_status(competence_occupation__hybrid_occupation, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(competence_occupation__hybrid_occupation, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(competence_occupation__hybrid_occupation, 'none', 1).
narrative_ontology:epsilon_provenance(competence_occupation__hybrid_occupation, 0.62, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(competence_occupation__hybrid_occupation_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(competence_occupation__hybrid_occupation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(competence_occupation__hybrid_occupation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) is authored high because the perpetual multi-mechanism requirement consumes resources well beyond any demonstrated marginal safety return, and the 'no consensus' condition prevents optimization that would cap costs. Suppression (0.58) reflects that alternative training configurations (lighter, single-mechanism, or risk-based) are institutionally barred by regulatory mandate and professional norms. Theater ratio (0.45) captures the performative dimension: a significant fraction of activity consists of box-checking and redundant audits that reassure more than they improve. Accessibility collapse (0.48) is moderate: alternatives are thinkable but cannot get regulatory traction. Resistance (0.42) is moderate: operators and some organizations resist the load, but coalition formation is hindered by safety culture stigma against 'cutting corners'.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (regulatory authorities) and beneficiary seats (vendors, auditors) experience the constraint as legitimate expertise-based coordination. The payer seats (operators, organizations) experience the same structure as an ever-expanding, non-optimizable compliance burden. The engine computes this divergence from structural data: agenda_setters have analytical exit and institutional power; payers have constrained exit and bear the costs.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (training_system_vendors, external_safety_auditors) receive low directionality because the constraint subsidizes their revenue and authority. Payers (frontline_operators, operating_organizations) receive high directionality because the constraint extracts time, attention, and budget from them. Operating_organizations are annotated with a secondary beneficiary role to capture their liability-reduction gain, but their primary structural position in the victim array drives their directional placement toward the target end. Regulatory authorities are agenda_setters, not beneficiaries, and their directionality is analytical.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resists classification as pure rope because the lack of consensus on optimal configuration means costs are not bounded by a clear coordination function; it also resists classification as pure snare because catastrophic skill decay is a genuine failure mode that single-mechanism training has historically failed to prevent. The tangled_rope classification captures both the real coordination (safety) and the asymmetric extraction (perpetual vendor-captured compliance). If the founding problem (competence gaps causing catastrophes) were dead, the constraint would drift toward piton; here the founding problem is contested, keeping it in tangled_rope territory.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'Is the hybrid-occupation reading the correct decomposition of the competence kernel, or does one of the sibling readings (simulation-sufficiency or real-incident-necessity) better capture the actual constraint structure?',
    'Empirical comparison of safety outcomes across jurisdictions or organizations adopting different readings.',
    'If simulation_sufficiency were true, epsilon would drop significantly (less extraction); if real_incident_necessity were true, epsilon would spike (catastrophic extraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Uncertainty about which reading of the competence occupation kernel is structurally accurate').

omega_variable(
    competence_decay_naturalness,
    'Does skill decay in high-reliability occupations constitute a natural law requiring the hybrid constraint, or is the decay curve itself partially constructed by the training regime''s design?',
    'Longitudinal studies of operators trained under minimal vs. hybrid regimes measuring actual performance decay.',
    'If decay is largely constructed by the regime, the constraint is more extractive than its natural-law framing suggests.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(competence_decay_naturalness, empirical, 'Whether skill decay is intrinsic or training-regime-dependent').

omega_variable(
    enforcement_vs_professional_norm,
    'Does the constraint persist primarily through regulatory enforcement or through professional identity fusion that makes operators self-enforce?',
    'Compare continuation rates in jurisdictions with identical regulations but different professional cultures.',
    'If identity-locked, effective extraction is higher than structural suppression suggests; if purely regulatory, reform is easier.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_vs_professional_norm, empirical, 'Regulatory enforcement vs internalized professional norm as suppression driver').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(competence_occupation__hybrid_occupation, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comp_tr_t0, competence_occupation__hybrid_occupation, theater_ratio, 0, 0.3).
narrative_ontology:measurement(comp_tr_t5, competence_occupation__hybrid_occupation, theater_ratio, 5, 0.35).
narrative_ontology:measurement(comp_tr_t10, competence_occupation__hybrid_occupation, theater_ratio, 10, 0.38).
narrative_ontology:measurement(comp_tr_t15, competence_occupation__hybrid_occupation, theater_ratio, 15, 0.42).
narrative_ontology:measurement(comp_tr_t20, competence_occupation__hybrid_occupation, theater_ratio, 20, 0.44).
narrative_ontology:measurement(comp_tr_t25, competence_occupation__hybrid_occupation, theater_ratio, 25, 0.45).

% Extraction over time
narrative_ontology:measurement(comp_be_t0, competence_occupation__hybrid_occupation, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(comp_be_t5, competence_occupation__hybrid_occupation, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(comp_be_t10, competence_occupation__hybrid_occupation, base_extractiveness, 10, 0.56).
narrative_ontology:measurement(comp_be_t15, competence_occupation__hybrid_occupation, base_extractiveness, 15, 0.6).
narrative_ontology:measurement(comp_be_t20, competence_occupation__hybrid_occupation, base_extractiveness, 20, 0.61).
narrative_ontology:measurement(comp_be_t25, competence_occupation__hybrid_occupation, base_extractiveness, 25, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(comp_su_t0, competence_occupation__hybrid_occupation, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(comp_su_t5, competence_occupation__hybrid_occupation, suppression_requirement, 5, 0.46).
narrative_ontology:measurement(comp_su_t10, competence_occupation__hybrid_occupation, suppression_requirement, 10, 0.5).
narrative_ontology:measurement(comp_su_t15, competence_occupation__hybrid_occupation, suppression_requirement, 15, 0.54).
narrative_ontology:measurement(comp_su_t20, competence_occupation__hybrid_occupation, suppression_requirement, 20, 0.56).
narrative_ontology:measurement(comp_su_t25, competence_occupation__hybrid_occupation, suppression_requirement, 25, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(competence_occupation__hybrid_occupation, identity_coordination).
narrative_ontology:affects_constraint(competence_occupation__hybrid_occupation, simulation_sufficiency).
narrative_ontology:affects_constraint(competence_occupation__hybrid_occupation, real_incident_necessity).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the competence_occupation kernel, decomposed per the epsilon-invariance principle because the sibling readings instantiate structurally distinct claims with different epsilon values and stakeholder structures.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
