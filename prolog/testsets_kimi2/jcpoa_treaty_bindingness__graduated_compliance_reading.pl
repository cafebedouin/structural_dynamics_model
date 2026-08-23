% ============================================================================
% CONSTRAINT STORY: jcpoa_treaty_bindingness__graduated_compliance_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jcpoa_treaty_bindingness__graduated_compliance_reading, []).

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
 *   constraint_id: jcpoa_treaty_bindingness__graduated_compliance_reading
 *   human_readable: JCPOA Graduated Compliance Bindingness Reading
 *   domain: international_law/nuclear_non_proliferation/treaty_compliance
 *
 * SUMMARY:
 *   This is the graduated_compliance_reading of the jcpoa_treaty_bindingness
 *   kernel. It treats the JCPOA not as a binding multilateral treaty
 *   requiring consensus-based modification, nor as a provisional transaction
 *   voidable upon unilateral bad-faith determination, but as a scaled
 *   reciprocal commitment. Under this reading, the Joint Commission
 *   calibrates sanctions relief withdrawal proportionally to Iranian
 *   enrichment increases, and dispute resolution prioritizes de-escalation
 *   over formal legal closure. The constraint coordinates nuclear
 *   non-proliferation and partial economic re-engagement while asymmetrically
 *   extracting sovereignty costs from Iran.
 *
 * KEY AGENTS:
 *   - Joint Commission / P5+1: agenda setter (institutional/global) â administers graduated compliance and dispute resolution
 *   - Iranian state: payer (powerful/national) â bears enrichment restrictions, monitoring costs, and partial relief withdrawal
 *   - Economic engagement actors: beneficiary (organized/global) â collect partial trade access and risk-adjusted re-engagement
 *   - Non-proliferation regime: beneficiary (institutional/global) â collects verification norms and NPT reinforcement
 *   - IAEA verification body: observer (institutional/global) â provides technical monitoring without setting political responses
 *   - Unilateralist hardliners: excluded (powerful/national) â oppose graduated framework and are kept outside Joint Commission architecture
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jcpoa_treaty_bindingness__graduated_compliance_reading, 0.68).
domain_priors:suppression_score(jcpoa_treaty_bindingness__graduated_compliance_reading, 0.62).
domain_priors:theater_ratio(jcpoa_treaty_bindingness__graduated_compliance_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__graduated_compliance_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__graduated_compliance_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__graduated_compliance_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__graduated_compliance_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__graduated_compliance_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jcpoa_treaty_bindingness__graduated_compliance_reading, tangled_rope).
narrative_ontology:human_readable(jcpoa_treaty_bindingness__graduated_compliance_reading, "JCPOA Graduated Compliance Bindingness Reading").
narrative_ontology:topic_domain(jcpoa_treaty_bindingness__graduated_compliance_reading, "international_law/nuclear_non_proliferation/treaty_compliance").

domain_priors:requires_active_enforcement(jcpoa_treaty_bindingness__graduated_compliance_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jcpoa_treaty_bindingness__graduated_compliance_reading, 'c8cd3e99-02e2-40c9-b8f7-3e9c1c76957b').
narrative_ontology:cs_kernel_codification('c8cd3e99-02e2-40c9-b8f7-3e9c1c76957b', formalized).
narrative_ontology:cs_authority_grounding('c8cd3e99-02e2-40c9-b8f7-3e9c1c76957b', practice).
narrative_ontology:cs_interpretation_layer_present('c8cd3e99-02e2-40c9-b8f7-3e9c1c76957b').
narrative_ontology:cs_reading_relation('c8cd3e99-02e2-40c9-b8f7-3e9c1c76957b', jcpoa_treaty_bindingness__binding_multilateral_reading, coexists_with).
narrative_ontology:cs_reading_relation('c8cd3e99-02e2-40c9-b8f7-3e9c1c76957b', jcpoa_treaty_bindingness__transactional_provisional_reading, coexists_with).
narrative_ontology:cs_axiom('c8cd3e99-02e2-40c9-b8f7-3e9c1c76957b', foundational, proportional_sanctions_relief_legitimate).
narrative_ontology:cs_axiom_status(proportional_sanctions_relief_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('c8cd3e99-02e2-40c9-b8f7-3e9c1c76957b', proportional_sanctions_relief_legitimate, conventional).
narrative_ontology:cs_axiom('c8cd3e99-02e2-40c9-b8f7-3e9c1c76957b', foundational, enrichment_quantification_objective).
narrative_ontology:cs_axiom_status(enrichment_quantification_objective, holdable).
narrative_ontology:cs_axiom_grounding('c8cd3e99-02e2-40c9-b8f7-3e9c1c76957b', enrichment_quantification_objective, empirically_contingent).
narrative_ontology:cs_reference_frame('c8cd3e99-02e2-40c9-b8f7-3e9c1c76957b', reciprocal_commitment_equilibrium).
narrative_ontology:cs_drift_state('c8cd3e99-02e2-40c9-b8f7-3e9c1c76957b', post_us_withdrawal_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('c8cd3e99-02e2-40c9-b8f7-3e9c1c76957b', '').
narrative_ontology:cs_kernel_id(jcpoa_treaty_bindingness__graduated_compliance_reading, jcpoa_treaty_bindingness).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__graduated_compliance_reading, economic_engagement_actors).
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__graduated_compliance_reading, non_proliferation_regime).
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__graduated_compliance_reading, pragmatic_diplomacy_advocates).
narrative_ontology:constraint_victim(jcpoa_treaty_bindingness__graduated_compliance_reading, iranian_state).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the JCPOA's graduated compliance assessment through the Joint Commission, calibrating sanctions relief withdrawal proportionally to verified Iranian enrichment levels and managing the dispute resolution mechanism that prioritizes de-escalation over formal legal closure. Exit is constrained by the political cost of abandoning the only shared multilateral framework.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__graduated_compliance_reading, joint_commission_p5_plus_one, agenda_setter,
    institutional, generational, constrained, global).

% Accepts quantified and verified limits on centrifuge numbers, enrichment levels, and stockpiles in exchange for graded, reversible sanctions relief. Bears sovereignty costs of intrusive IAEA monitoring and faces partial relief withdrawal if enrichment increases. Exit means returning to full multilateral sanctions and isolation.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__graduated_compliance_reading, iranian_state, payer,
    powerful, generational, constrained, national).

% European and Asian firms and investors that resumed partial trade, banking, and energy transactions with Iran under relief provisions. They benefit from calibrated re-engagement and risk-adjusted market access, though they remain exposed to snapback uncertainty and US secondary sanctions.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__graduated_compliance_reading, economic_engagement_actors, beneficiary,
    organized, biographical, mobile, global).

% International non-proliferation institutions, norms, and verification frameworks that benefit from the continued existence of a monitored, quantified constraint on Iranian nuclear expansion, reinforcing the broader Treaty on the Non-Proliferation of Nuclear Weapons bargain.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__graduated_compliance_reading, non_proliferation_regime, beneficiary,
    institutional, civilizational, analytical, global).

% Diplomats, policy experts, and Track-II negotiators who advocate for proportional response and de-escalation. They benefit professionally and politically from the viability of a graduated framework that sustains dialogue without requiring binary success or failure.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__graduated_compliance_reading, pragmatic_diplomacy_advocates, beneficiary,
    organized, biographical, mobile, global).

% Conducts technical monitoring and verification under the Additional Protocol and JCPOA-specific measures, providing fuel-cycle data to the Joint Commission. It does not set political responses but its assessments feed directly into the graduated calibration mechanism.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__graduated_compliance_reading, iaea_verification_body, observer,
    institutional, generational, analytical, global).

% Actors in the United States, Israel, and elsewhere who reject any sanctioned Iranian enrichment and favor maximum pressure or military options. They are structurally excluded from the JCPOA's Joint Commission dispute resolution architecture but exert persistent external political pressure on its parties.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__graduated_compliance_reading, unilateralist_hardliners, excluded,
    powerful, biographical, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a collective-action problem among the P5+1, the EU, and Iran: how to verifiably limit Iranian nuclear enrichment capacity while allowing partial economic re-engagement, without forcing a choice between accepting an Iranian bomb and launching preventive military strikes.
% TRANSFER_FUNCTION: Moves sanctions relief and economic access from the international sanctions architecture toward Iran in proportion to verified centrifuge and stockpile reductions; moves compliance costs, sovereignty restrictions, and monitoring burdens from Iran toward the IAEA and Joint Commission apparatus.
% ABSENT_VOICES: Unilateralist hardliners who reject any Iranian enrichment and Iranian sovereignty-maximalists who reject intrusive monitoring and graded relief are both outside the Joint Commission's dispute resolution room; their exclusion shapes the consensus the framework claims.
% DISAPPEARANCE_RATIONALE: If the graduated compliance mechanism vanished overnight, Iran would likely accelerate enrichment without a calibrated proportional response protocol, the P5+1 would lose a shared framework for coordinated retaliation, economic actors would face total sanctions uncertainty rather than graded risk, and regional actors would revert to unilateral threat assessments.
% FOUNDING_PROBLEM: The Iranian nuclear program had advanced to a threshold where breakout timelines were measured in months; the P5+1 needed a verifiable alternative to either accepting a nuclear-armed Iran or launching preventive military strikes.
% FOUNDING_PROBLEM_CORROBORATION: The IAEA and US intelligence community attested the short breakout timeline in 2015. Israeli intelligence and Gulf state actors corroborate the threat but dispute that the JCPOA solved it; independent security scholars outside the direct beneficiary set are split on whether the founding problem is now worse due to Iranian counter-escalation.
narrative_ontology:disappearance_verdict(jcpoa_treaty_bindingness__graduated_compliance_reading, world_rearranges).
narrative_ontology:founding_problem_status(jcpoa_treaty_bindingness__graduated_compliance_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jcpoa_treaty_bindingness__graduated_compliance_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(jcpoa_treaty_bindingness__graduated_compliance_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jcpoa_treaty_bindingness__graduated_compliance_reading, 0.68, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jcpoa_treaty_bindingness__graduated_compliance_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jcpoa_treaty_bindingness__graduated_compliance_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jcpoa_treaty_bindingness__graduated_compliance_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-high (0.68) because the arrangement structurally extracts Iranian nuclear sovereignty through intrusive monitoring and graded punishment, even as it delivers partial relief. Suppression is moderate-high (0.62) because the mechanism requires active IAEA access, snapback threats, and unilateral secondary sanctions to hold. Theater ratio is moderate (0.45) because compliance assessments involve significant diplomatic performance even as underlying limits erode. Accessibility collapse is moderate (0.40): alternatives such as military strikes or full diplomatic normalization remain live concepts. Resistance is moderate-high (0.58) from Iranian hardliners, US unilateralists, and external actors. All measurement series share a single time grid to prevent misalignment artifacts.
 *
 * PERSPECTIVAL GAP:
 *   The Iranian state experiences the constraint as sovereignty extraction with partial, reversible relief; the P5+1 and non-proliferation community experience it as a successful coordination mechanism that prevents war; economic actors experience graded risk rather than binary exclusion. The engine computes these divergences from the asymmetry in power, exit options, and declared beneficiary/victim roles.
 *
 * DIRECTIONALITY LOGIC:
 *   The Iranian state is the primary target: it accepts quantified limits and intrusive monitoring while facing relief withdrawal if enrichment increases, placing its directionality near the target end. The Joint Commission / P5+1 is the agenda setter and net beneficiary of non-proliferation stability, placing it near the beneficiary end. Economic actors, pragmatic diplomacy advocates, and the non-proliferation regime are beneficiaries with low directionality. The IAEA sits near the analytical middle because it provides technical labor without capturing political gains.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents mislabeling as pure extraction because it genuinely coordinates a collective-action problem: preventing Iranian nuclear breakout without military confrontation, delivering verifiable limits and partial relief. It prevents mislabeling as pure coordination because the extraction is asymmetricâIran bears sovereignty and monitoring costs that the P5+1 does not, and the relief it receives is partial, calibrated, and reversible. The graduated mechanism is the coordination story; the sovereignty restriction and relief withdrawal asymmetry are the extraction story. Both pass through the same institutional structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    graduation_vs_erosion,
    'Is the graduated enforcement mechanism still operating proportionally, or has it collapsed into binary punishment and relief after the US unilateral withdrawal?',
    'Compare compliance responses before and after 2018: if sanctions relief withdrawal and Iranian countermeasures are no longer proportionally indexed to specific enrichment increments, the graduated mechanism has eroded into a binary dynamic.',
    'If eroded, the constraint loses its coordination function and approaches a snare or piton, where extraction persists without proportional structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(graduation_vs_erosion, empirical, 'Whether proportional graduation has collapsed into binary enforcement').

omega_variable(
    sovereignty_cost_commensurability,
    'Are the sovereignty restrictions and monitoring burdens imposed on Iran commensurate with the non-proliferation value generated, or is the asymmetry structurally excessive?',
    'Independent strategic assessment of breakout-timeline reduction per unit of Iranian sovereignty ceded, benchmarked against alternative non-proliferation frameworks.',
    'If the asymmetry is excessive, the constraint''s classification leans more heavily toward extraction; if commensurate, the coordination function dominates.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sovereignty_cost_commensurability, conceptual, 'Whether Iranian sovereignty costs match generated non-proliferation value').

omega_variable(
    snapback_mechanism_viability,
    'Does the UN snapback procedure retain functional coercive force, or has it become diplomatic theater after the US withdrawal and challenge to its standing?',
    'Track whether JCPOA participants and third parties treat the snapback threat as credible in their risk calculus, or whether they now discount it and rely solely on unilateral US sanctions.',
    'If non-credible, the constraint''s active enforcement relies on unilateral US power rather than multilateral architecture, shifting the constraint toward extraction by a single agenda-setter.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(snapback_mechanism_viability, empirical, 'Whether the UN snapback mechanism remains credible or has become theater').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jcpoa_treaty_bindingness__graduated_compliance_reading, 0, 8).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jcpoa_grad_tr_t0, jcpoa_treaty_bindingness__graduated_compliance_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(jcpoa_grad_tr_t1, jcpoa_treaty_bindingness__graduated_compliance_reading, theater_ratio, 1, 0.22).
narrative_ontology:measurement(jcpoa_grad_tr_t2, jcpoa_treaty_bindingness__graduated_compliance_reading, theater_ratio, 2, 0.25).
narrative_ontology:measurement(jcpoa_grad_tr_t3, jcpoa_treaty_bindingness__graduated_compliance_reading, theater_ratio, 3, 0.35).
narrative_ontology:measurement(jcpoa_grad_tr_t4, jcpoa_treaty_bindingness__graduated_compliance_reading, theater_ratio, 4, 0.4).
narrative_ontology:measurement(jcpoa_grad_tr_t5, jcpoa_treaty_bindingness__graduated_compliance_reading, theater_ratio, 5, 0.45).
narrative_ontology:measurement(jcpoa_grad_tr_t6, jcpoa_treaty_bindingness__graduated_compliance_reading, theater_ratio, 6, 0.48).
narrative_ontology:measurement(jcpoa_grad_tr_t7, jcpoa_treaty_bindingness__graduated_compliance_reading, theater_ratio, 7, 0.5).
narrative_ontology:measurement(jcpoa_grad_tr_t8, jcpoa_treaty_bindingness__graduated_compliance_reading, theater_ratio, 8, 0.45).

% Extraction over time
narrative_ontology:measurement(jcpoa_grad_be_t0, jcpoa_treaty_bindingness__graduated_compliance_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(jcpoa_grad_be_t1, jcpoa_treaty_bindingness__graduated_compliance_reading, base_extractiveness, 1, 0.42).
narrative_ontology:measurement(jcpoa_grad_be_t2, jcpoa_treaty_bindingness__graduated_compliance_reading, base_extractiveness, 2, 0.4).
narrative_ontology:measurement(jcpoa_grad_be_t3, jcpoa_treaty_bindingness__graduated_compliance_reading, base_extractiveness, 3, 0.58).
narrative_ontology:measurement(jcpoa_grad_be_t4, jcpoa_treaty_bindingness__graduated_compliance_reading, base_extractiveness, 4, 0.65).
narrative_ontology:measurement(jcpoa_grad_be_t5, jcpoa_treaty_bindingness__graduated_compliance_reading, base_extractiveness, 5, 0.7).
narrative_ontology:measurement(jcpoa_grad_be_t6, jcpoa_treaty_bindingness__graduated_compliance_reading, base_extractiveness, 6, 0.65).
narrative_ontology:measurement(jcpoa_grad_be_t7, jcpoa_treaty_bindingness__graduated_compliance_reading, base_extractiveness, 7, 0.62).
narrative_ontology:measurement(jcpoa_grad_be_t8, jcpoa_treaty_bindingness__graduated_compliance_reading, base_extractiveness, 8, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(jcpoa_grad_su_t0, jcpoa_treaty_bindingness__graduated_compliance_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(jcpoa_grad_su_t1, jcpoa_treaty_bindingness__graduated_compliance_reading, suppression_requirement, 1, 0.32).
narrative_ontology:measurement(jcpoa_grad_su_t2, jcpoa_treaty_bindingness__graduated_compliance_reading, suppression_requirement, 2, 0.3).
narrative_ontology:measurement(jcpoa_grad_su_t3, jcpoa_treaty_bindingness__graduated_compliance_reading, suppression_requirement, 3, 0.55).
narrative_ontology:measurement(jcpoa_grad_su_t4, jcpoa_treaty_bindingness__graduated_compliance_reading, suppression_requirement, 4, 0.6).
narrative_ontology:measurement(jcpoa_grad_su_t5, jcpoa_treaty_bindingness__graduated_compliance_reading, suppression_requirement, 5, 0.65).
narrative_ontology:measurement(jcpoa_grad_su_t6, jcpoa_treaty_bindingness__graduated_compliance_reading, suppression_requirement, 6, 0.6).
narrative_ontology:measurement(jcpoa_grad_su_t7, jcpoa_treaty_bindingness__graduated_compliance_reading, suppression_requirement, 7, 0.58).
narrative_ontology:measurement(jcpoa_grad_su_t8, jcpoa_treaty_bindingness__graduated_compliance_reading, suppression_requirement, 8, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
