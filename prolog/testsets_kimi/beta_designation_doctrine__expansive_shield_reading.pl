% ============================================================================
% CONSTRAINT STORY: beta_designation_doctrine__expansive_shield_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_beta_designation_doctrine__expansive_shield_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: beta_designation_doctrine__expansive_shield_reading
 *   human_readable: Expansive Beta Designation Doctrine (Comprehensive Liability Shield)
 *   domain: technology_law/software_liability
 *
 * SUMMARY:
 *   This constraint instantiates the expansive_shield_reading of the
 *   beta_designation_doctrine kernel, under which the beta label is
 *   interpreted as a comprehensive, indefinite liability waiver applicable to
 *   all software contexts. Sibling readings include narrow_warning_reading
 *   (time-bounded testing disclosure preserving base liability) and
 *   severity_carve_out_reading (unavailable for life-safety or critical
 *   systems). In this reading, developers externalize all defect costs to
 *   users; users enter the victim set; and there are no temporal or severity
 *   boundaries.
 *
 * KEY AGENTS:
 *   - Software vendors: Primary beneficiaries (powerful/arbitrage) â capture risk externalization through boilerplate EULAs.
 *   - End users: Primary victims (powerless/trapped) â bear defect costs without recourse.
 *   - Judiciary: Agenda setter (institutional/analytical) â enforces the expansive reading as contract doctrine.
 *   - Consumer protection advocates: Excluded voice (moderate/constrained) â object but are preempted by judicial interpretation.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(beta_designation_doctrine__expansive_shield_reading, 0.85).
domain_priors:suppression_score(beta_designation_doctrine__expansive_shield_reading, 0.8).
domain_priors:theater_ratio(beta_designation_doctrine__expansive_shield_reading, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(beta_designation_doctrine__expansive_shield_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(beta_designation_doctrine__expansive_shield_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(beta_designation_doctrine__expansive_shield_reading, theater_ratio, 0.65).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(beta_designation_doctrine__expansive_shield_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(beta_designation_doctrine__expansive_shield_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(beta_designation_doctrine__expansive_shield_reading, snare).
narrative_ontology:human_readable(beta_designation_doctrine__expansive_shield_reading, "Expansive Beta Designation Doctrine (Comprehensive Liability Shield)").
narrative_ontology:topic_domain(beta_designation_doctrine__expansive_shield_reading, "technology_law/software_liability").

domain_priors:requires_active_enforcement(beta_designation_doctrine__expansive_shield_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(beta_designation_doctrine__expansive_shield_reading, 'c9b1882a-7001-4f3c-930d-0215cde4afcb').
narrative_ontology:cs_kernel_codification('c9b1882a-7001-4f3c-930d-0215cde4afcb', distributed).
narrative_ontology:cs_authority_grounding('c9b1882a-7001-4f3c-930d-0215cde4afcb', distributed).
narrative_ontology:cs_reading_relation('c9b1882a-7001-4f3c-930d-0215cde4afcb', beta_designation_doctrine__narrow_warning_reading, coexists_with).
narrative_ontology:cs_reading_relation('c9b1882a-7001-4f3c-930d-0215cde4afcb', beta_designation_doctrine__severity_carve_out_reading, coexists_with).
narrative_ontology:cs_axiom('c9b1882a-7001-4f3c-930d-0215cde4afcb', foundational, beta_label_comprehensive_liability_waiver).
narrative_ontology:cs_axiom_status(beta_label_comprehensive_liability_waiver, holdable).
narrative_ontology:cs_axiom_grounding('c9b1882a-7001-4f3c-930d-0215cde4afcb', beta_label_comprehensive_liability_waiver, conventional).
narrative_ontology:cs_axiom('c9b1882a-7001-4f3c-930d-0215cde4afcb', foundational, indefinite_beta_duration_permissible).
narrative_ontology:cs_axiom_status(indefinite_beta_duration_permissible, holdable).
narrative_ontology:cs_axiom_grounding('c9b1882a-7001-4f3c-930d-0215cde4afcb', indefinite_beta_duration_permissible, conventional).
narrative_ontology:cs_reference_frame('c9b1882a-7001-4f3c-930d-0215cde4afcb', unlimited_vendor_immunity_framework).
narrative_ontology:cs_drift_state('c9b1882a-7001-4f3c-930d-0215cde4afcb', contemporary_consumer_protection_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('c9b1882a-7001-4f3c-930d-0215cde4afcb', '').
narrative_ontology:cs_kernel_id(beta_designation_doctrine__expansive_shield_reading, beta_designation_doctrine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(beta_designation_doctrine__expansive_shield_reading, software_vendors).
narrative_ontology:constraint_victim(beta_designation_doctrine__expansive_shield_reading, end_users).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Apply the beta designation to software products across all contexts and durations to extinguish liability for defects. Capture the gains of risk externalization through lower insurance costs, reduced quality-assurance expenditure, and insulation from tort and contract claims.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__expansive_shield_reading, software_vendors, beneficiary,
    powerful, biographical, arbitrage, global).

% Use software labeled beta in production, commercial, and personal contexts without meaningful alternative. Bear the full cost of data loss, security breaches, physical harm, and financial loss caused by defects; judicial interpretation bars all recovery.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__expansive_shield_reading, end_users, payer,
    powerless, biographical, trapped, global).

% Interprets contract doctrine and the beta designation as a comprehensive liability waiver regardless of actual testing phase, defect severity, or user understanding. Enforces the waiver by dismissing product liability and consumer protection claims at the pleading stage.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__expansive_shield_reading, judiciary, agenda_setter,
    institutional, generational, analytical, national).

% Argue that indefinite beta status nullifies consumer protection law and tort remedies. Structurally excluded from EULA negotiations and preempted by judicial adoption of the expansive reading; their objections are heard in amicus briefs but rarely alter outcomes.
narrative_ontology:constraint_stakeholder(beta_designation_doctrine__expansive_shield_reading, consumer_protection_advocates, excluded,
    moderate, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(beta_designation_doctrine__expansive_shield_reading, software_vendors).
narrative_ontology:fixing_cost_class(beta_designation_doctrine__expansive_shield_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: No genuine coordination problem is solved under this reading; the nominal testing-status disclosure is severed from user choice by indefinite duration and universal applicability, leaving only the externalization of defect costs.
% TRANSFER_FUNCTION: Moves all liability for software defectsâincluding production-grade, mature, and critical-system failuresâfrom vendors to end users, regardless of actual testing status or user awareness.
% ABSENT_VOICES: End users who lack bargaining power and legal resources; consumer protection agencies; product liability plaintiffs' bar; injured parties in life-safety or financial contexts who are bound by boilerplate terms they did not negotiate.
% DISAPPEARANCE_RATIONALE: If the expansive shield vanished, vendors would internalize liability risk, altering software pricing, quality assurance investment, insurance markets, and EULA design; users would regain tort and contractual remedies.
% FOUNDING_PROBLEM: How to protect genuine pre-release software testing from liability when failure is expected and user participation is voluntary and informed.
% FOUNDING_PROBLEM_CORROBORATION: Consumer protection scholarship and narrow-reading jurists attest the original testing-phase problem is solved by time-bounded doctrines; the expansive reading is defended only by vendor-side trade associations and technology sector briefs, with no independent corroboration of its continued necessity.
narrative_ontology:disappearance_verdict(beta_designation_doctrine__expansive_shield_reading, world_rearranges).
narrative_ontology:founding_problem_status(beta_designation_doctrine__expansive_shield_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(beta_designation_doctrine__expansive_shield_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(beta_designation_doctrine__expansive_shield_reading, 'none', 1).
narrative_ontology:epsilon_provenance(beta_designation_doctrine__expansive_shield_reading, 0.85, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(beta_designation_doctrine__expansive_shield_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(beta_designation_doctrine__expansive_shield_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(beta_designation_doctrine__expansive_shield_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness is high (0.85) because the doctrine transfers all defect liability to users irrespective of actual maturity or context. Suppression is high (0.80) because the waiver preempts consumer protection and tort remedies that would otherwise be available. Theater ratio is substantial (0.65) because the beta label is performative: it signals testing status while operating as a permanent legal shield. Accessibility collapse is high (0.75) because once the doctrine is accepted, users have no alternative legal pathway to recovery. Resistance is moderate (0.45) because consumer advocates and some regulators contest the reading but lack institutional leverage to reverse it.
 *
 * PERSPECTIVAL GAP:
 *   The vendor seat experiences the constraint as legitimate risk management and innovation policy; the user seat experiences it as unilateral cost imposition. The judiciary sees doctrinal coherence in contract freedom, while excluded consumer advocates see the evacuation of public-law protections. The engine computes these divergences from the structural data: low directionality for vendors, high directionality for users.
 *
 * DIRECTIONALITY LOGIC:
 *   Software vendors are structural beneficiaries (low d): the constraint subsidizes their risk exposure. End users are structural targets (high d): they absorb the extraction directly. The judiciary administers the constraint without collecting from it; its directionality is near-neutral but leans toward the agenda-setting function. No override is needed because beneficiary/victim declarations and exit options already map the relationships correctly.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents mislabeling the doctrine as a rope (innovation coordination) by tracing the R5 genealogy: the founding problem was protecting genuine, time-limited testing. Under the expansive reading, that problem is deadâthe doctrine now shields production software indefinitely. The mismatch between founding_problem_status=dead and disappearance_verdict=world_rearranges flags the arrangement as a zombie constraint, not a living coordination mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    expansive_reading_capture_hypothesis,
    'Is the expansive reading a good-faith interpretive extension of contract doctrine, or a deliberate doctrinal capture by vendor-side interests to externalize liability?',
    'Historical analysis of trade-association amicus briefs, legislative lobbying records, and judicial opinion citation networks to identify interest-group provenance.',
    'If capture is established, the snare classification is strengthened; if good-faith interpretive drift, the classification might shift toward tangled rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(expansive_reading_capture_hypothesis, conceptual, 'Whether the expansive reading represents vendor capture or good-faith doctrinal evolution.').

omega_variable(
    user_consent_comprehension,
    'Do end users comprehend that a beta designation signifies a comprehensive, indefinite waiver of liability for all defect types?',
    'Empirical user studies of EULA comprehension and behavioral experiments testing liability expectations under beta labels.',
    'If users do not comprehend the waiver, the constraint''s effective suppression is higher than the structural measure suggests, deepening the snare classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(user_consent_comprehension, empirical, 'Whether user consent to beta terms is informed or illusory.').

omega_variable(
    temporal_boundary_enforceability,
    'At what temporal or severity boundary does a beta designation cease to be a genuine testing disclosure and become a liability shield?',
    'Comparative legal analysis of jurisdictions that enforce temporal limits or severity carve-outs on beta waivers.',
    'A clear boundary would validate the narrow reading as a distinct constraint; the absence of any enforceable boundary confirms the expansive reading as a stable snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(temporal_boundary_enforceability, conceptual, 'Boundary between genuine testing disclosure and liability shield.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(beta_designation_doctrine__expansive_shield_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(beta_exp_shield_tr_t0, beta_designation_doctrine__expansive_shield_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(beta_exp_shield_tr_t5, beta_designation_doctrine__expansive_shield_reading, theater_ratio, 5, 0.3).
narrative_ontology:measurement(beta_exp_shield_tr_t10, beta_designation_doctrine__expansive_shield_reading, theater_ratio, 10, 0.45).
narrative_ontology:measurement(beta_exp_shield_tr_t15, beta_designation_doctrine__expansive_shield_reading, theater_ratio, 15, 0.55).
narrative_ontology:measurement(beta_exp_shield_tr_t20, beta_designation_doctrine__expansive_shield_reading, theater_ratio, 20, 0.62).
narrative_ontology:measurement(beta_exp_shield_tr_t25, beta_designation_doctrine__expansive_shield_reading, theater_ratio, 25, 0.65).

% Extraction over time
narrative_ontology:measurement(beta_exp_shield_be_t0, beta_designation_doctrine__expansive_shield_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(beta_exp_shield_be_t5, beta_designation_doctrine__expansive_shield_reading, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(beta_exp_shield_be_t10, beta_designation_doctrine__expansive_shield_reading, base_extractiveness, 10, 0.65).
narrative_ontology:measurement(beta_exp_shield_be_t15, beta_designation_doctrine__expansive_shield_reading, base_extractiveness, 15, 0.75).
narrative_ontology:measurement(beta_exp_shield_be_t20, beta_designation_doctrine__expansive_shield_reading, base_extractiveness, 20, 0.82).
narrative_ontology:measurement(beta_exp_shield_be_t25, beta_designation_doctrine__expansive_shield_reading, base_extractiveness, 25, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(beta_exp_shield_su_t0, beta_designation_doctrine__expansive_shield_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(beta_exp_shield_su_t5, beta_designation_doctrine__expansive_shield_reading, suppression_requirement, 5, 0.5).
narrative_ontology:measurement(beta_exp_shield_su_t10, beta_designation_doctrine__expansive_shield_reading, suppression_requirement, 10, 0.6).
narrative_ontology:measurement(beta_exp_shield_su_t15, beta_designation_doctrine__expansive_shield_reading, suppression_requirement, 15, 0.7).
narrative_ontology:measurement(beta_exp_shield_su_t20, beta_designation_doctrine__expansive_shield_reading, suppression_requirement, 20, 0.77).
narrative_ontology:measurement(beta_exp_shield_su_t25, beta_designation_doctrine__expansive_shield_reading, suppression_requirement, 25, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
