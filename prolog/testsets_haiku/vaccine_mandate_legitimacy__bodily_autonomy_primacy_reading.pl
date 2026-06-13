% ============================================================================
% CONSTRAINT STORY: vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, []).

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
    domain_priors:emerges_naturally/1,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading
 *   human_readable: Medical Self-Sovereignty Absolute (Bodily Autonomy Primacy Reading)
 *   domain: constitutional_law/bioethics/public_health
 *
 * SUMMARY:
 *   This constraint story instantiates ONE READING of the contested kernel
 *   'vaccine_mandate_legitimacy': the bodily_autonomy_primacy_reading. Under
 *   this reading, medical self-sovereignty is absolute — the state possesses
 *   no authority to coerce medical intervention regardless of epidemiological
 *   outcome. The reading grounds itself in a foundational principle: negative
 *   liberty (freedom from bodily violation) is categorically prior to
 *   collective health considerations. This story does NOT adjudicate between
 *   the three readings; it author the structural data for THIS reading as a
 *   pure constraint. The deliberate declaration of beneficiaries (liberty
 *   advocacy movements) on a claimed-mountain constraint triggers FSM
 *   (false-summit mountain) detection machinery — the story documents the
 *   ambiguity via omegas. The measurement series shows gradual uptake of the
 *   reading's doctrinal articulation (extracted from case law, advocacy
 *   growth, policy disputes) from 2020–2026 as the constraint became more
 *   explicitly formulated and defended.
 *
 * KEY AGENTS:
 *   - individual_vaccine_decision_maker: Rights-bearer whose autonomy the constraint protects (powerless, identity-locked to bodily integrity)
 *   - immunocompromised_population: Bears external harm when vaccination rates drop; victim under this reading (trapped, mortal exposure risk)
 *   - medical_liberty_advocacy_organizations: Institutionalize and defend the reading; structural beneficiary (organized, mobile)
 *   - state_coercive_apparatus: Prohibited from mandate authority; loses policy tools (institutional, constrained)
 *   - competing_constitutional_readers: Public-health and risk-stratification seats excluded from authority under this reading (institutional, analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, 0.19).
domain_priors:suppression_score(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, 0.08).
domain_priors:theater_ratio(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, extractiveness, 0.19).
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, mountain).
narrative_ontology:human_readable(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, "Medical Self-Sovereignty Absolute (Bodily Autonomy Primacy Reading)").
narrative_ontology:topic_domain(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, "constitutional_law/bioethics/public_health").

domain_priors:emerges_naturally(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, '26a8486e-cea8-4f2f-822d-d02fb2765051').
narrative_ontology:cs_kernel_codification('26a8486e-cea8-4f2f-822d-d02fb2765051', fixed_text).
narrative_ontology:cs_authority_grounding('26a8486e-cea8-4f2f-822d-d02fb2765051', lineage).
narrative_ontology:cs_interpretation_layer_present('26a8486e-cea8-4f2f-822d-d02fb2765051').
narrative_ontology:cs_reading_relation('26a8486e-cea8-4f2f-822d-d02fb2765051', vaccine_mandate_legitimacy__public_health_primacy_reading, forecloses).
narrative_ontology:cs_reading_relation('26a8486e-cea8-4f2f-822d-d02fb2765051', vaccine_mandate_legitimacy__risk_stratification_reading, forecloses).
narrative_ontology:cs_axiom('26a8486e-cea8-4f2f-822d-d02fb2765051', foundational, bodily_autonomy_categorically_prior).
narrative_ontology:cs_axiom_status(bodily_autonomy_categorically_prior, holdable).
narrative_ontology:cs_axiom_grounding('26a8486e-cea8-4f2f-822d-d02fb2765051', bodily_autonomy_categorically_prior, deontological).
narrative_ontology:cs_axiom('26a8486e-cea8-4f2f-822d-d02fb2765051', foundational, state_coercive_authority_over_medicine_impermissible).
narrative_ontology:cs_axiom_status(state_coercive_authority_over_medicine_impermissible, holdable).
narrative_ontology:cs_axiom_grounding('26a8486e-cea8-4f2f-822d-d02fb2765051', state_coercive_authority_over_medicine_impermissible, deontological).
narrative_ontology:cs_reference_frame('26a8486e-cea8-4f2f-822d-d02fb2765051', negative_liberty_doctrine).
narrative_ontology:cs_drift_state('26a8486e-cea8-4f2f-822d-d02fb2765051', contemporary_post_pandemic_era, gap(stable, minor, false)).
narrative_ontology:cs_created_at('26a8486e-cea8-4f2f-822d-d02fb2765051', '').
narrative_ontology:cs_kernel_id(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, vaccine_mandate_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, liberty_advocacy_movements).
narrative_ontology:constraint_beneficiary(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, individuals_asserting_bodily_autonomy).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading_tests).

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, ExtMetricName, E),
    domain_priors:suppression_score(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading),
    narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The authored metrics reflect the structure of a reading that claims naturality but benefits identifiable agents. Extractiveness is low (0.19) because the constraint does not extract resources or transfer them materially — it articulates a prohibition. Suppression is very low (0.08) because the reading does not require active coercion to persist; rather, it operates by denying the state a policy tool. Theater is correspondingly low (0.12): little performative activity is needed; the constraint's persistence depends on doctrinal acceptance, not on staging compliance. Accessibility_collapse is extremely high (0.92): once the autonomy principle is understood and accepted as foundational, alternatives (collective health, proportional mandates, risk stratification) become nearly inaccessible within this reading's framework — the principle is presented as foreclosing them. Resistance is high (0.71): public-health authorities, epidemiologists, and vulnerable populations actively resist the reading's application, especially during high-mortality periods. The metrics are independent of the claimed type: the story claims mountain (naturality), but the metrics describe a constraint that benefits specific beneficiaries and excludes competing readings. This divergence is the datum the corpus exists to measure.
 *
 * PERSPECTIVAL GAP:
 *   The three readings of this kernel diverge most sharply in their treatment of vulnerability. The bodily_autonomy_primacy_reading (this story) treats immunocompromised as victims bearing acceptable externalities. The public_health_primacy_reading would treat them as primary beneficiaries (protection justifies mandate). The risk_stratification_reading would treat them as a threshold case: proportional protection justified, blanket mandates not. These are not differences in fact or in measuring the same thing — they are differences in which seat receives priority and protection in the reading's framework.
 *
 * DIRECTIONALITY LOGIC:
 *   The engine's directionality derivation begins with beneficiary/victim declarations and modulates by power + exit. This reading declares liberty advocacy (organized, mobile) and refusers (powerless, identity-locked) as beneficiaries, and immunocompromised (powerless, trapped) as victims. From this, d-values flow: beneficiaries push toward d near 0.0 (subsidized), victims push toward d near 1.0 (targeted). The identity-lock exit option for individual refusers signals deep fusion (autonomy is identity) rather than economic constraint; the engine may weight this differently than constrained or mobile exits. No override is needed for the engine's chain to work correctly.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading does not face classic mandatrophy (function atrophy while structure persists) in the interval. The founding problem — establishing bodily autonomy as a constitutional limit on state coercion — remains live and actively defended. However, the analysis of mandatrophy illuminates a deeper tension: the reading's declared beneficiaries (liberty advocates) have a structural interest in the problem remaining contested and unresolved. If mandates were universally rejected and the principle accepted as settled, the advocacy function would dissolve. The constraint's persistence depends on an ongoing debate. This is not mandatrophy (function loss) but a related dynamic: the constraint's stability depends on contested legitimacy rather than on functional necessity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_doctrine,
    'Is bodily autonomy an inviolable natural law or a constructed constitutional doctrine that emerged from specific intellectual and legal traditions?',
    'Historical and comparative analysis: does the principle appear universally across human societies independent of Western liberal tradition, or is it contingent on Enlightenment-era philosophy and modern rights frameworks?',
    'If natural law, the constraint deserves mountain classification despite declared beneficiaries. If constructed doctrine, the beneficiary set (liberty advocates) undermines the naturality claim and points to FSM (false-summit mountain) reclassification to tangled_rope or snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_doctrine, conceptual, 'Whether bodily autonomy is a discovered natural principle or an invented doctrine with identifiable beneficiaries.').

omega_variable(
    externality_bearing_asymmetry,
    'Is the external harm to immunocompromised populations from increased infection risk (due to lower vaccination rates) a necessary consequence of the autonomy principle, or a contingent policy choice to ignore protective measures?',
    'Policy design analysis: can the state simultaneously respect bodily autonomy (no mandates) AND protect vulnerable populations through voluntary incentives, healthcare infrastructure, or targeted protection? Or does respecting autonomy logically entail accepting the externality?',
    'If protective measures are available, the constraint''s external cost is reduced and the reading becomes more defensible. If protection requires either coercion or re-prioritization of resources away from other populations, the externality is irreducible and the constraint forces a true tragic choice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(externality_bearing_asymmetry, empirical, 'Whether vulnerability externalities are necessary or contingent under this reading.').

omega_variable(
    kernel_reading_plurality,
    'Is the bodily autonomy primacy reading genuinely one incommensurable reading of a single kernel (vaccine mandate legitimacy), or does it represent a different kernel entirely (individual liberty vs. collective health)?',
    'Conceptual analysis of kernel definition: do the three readings (bodily autonomy, public-health primacy, risk stratification) all adjudicate the SAME question (what justifies a mandate?), or do they answer different questions (is autonomy sacrosanct? is health? is proportionality)?',
    'If incommensurable questions, the three constraints do not belong in a single kernel family — they should be treated as independent constraints with distinct ε values rather than as siblings. The kernel decomposition itself may be incorrect.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_plurality, conceptual, 'Whether the reading plurality represents genuine kernel ambiguity or categorical separation.').

omega_variable(
    captured_beneficiary_status,
    'Do liberty advocacy organizations genuinely benefit from the principle''s articulation, or do they perform a dissent function that would persist even if mandates were legalized?',
    'Institutional analysis: if mandate legality changed tomorrow, would liberty organizations dissolve or find new targets? Do they depend on the principle''s victory or on the existence of a liberty principle to defend?',
    'If dependent on principle victory, the beneficiary set is real and FSM triggers (false-summit detection). If dependent on role as dissent-defenders, they benefit from principle vindication but the benefit is generic (role maintenance) rather than specific to this constraint, and FSM does not trigger.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(captured_beneficiary_status, conceptual, 'Whether liberty advocacy organizations are beneficiaries of the principle or defenders of a structural role.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, 2020, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vacc_tr_t2020, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, theater_ratio, 2020, 0.05).
narrative_ontology:measurement(vacc_tr_t2021, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, theater_ratio, 2021, 0.08).
narrative_ontology:measurement(vacc_tr_t2022, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, theater_ratio, 2022, 0.1).
narrative_ontology:measurement(vacc_tr_t2023, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, theater_ratio, 2023, 0.11).
narrative_ontology:measurement(vacc_tr_t2024, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, theater_ratio, 2024, 0.12).
narrative_ontology:measurement(vacc_tr_t2026, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, theater_ratio, 2026, 0.12).

% Extraction over time
narrative_ontology:measurement(vacc_be_t2020, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, base_extractiveness, 2020, 0.08).
narrative_ontology:measurement(vacc_be_t2021, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, base_extractiveness, 2021, 0.12).
narrative_ontology:measurement(vacc_be_t2022, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, base_extractiveness, 2022, 0.15).
narrative_ontology:measurement(vacc_be_t2023, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, base_extractiveness, 2023, 0.18).
narrative_ontology:measurement(vacc_be_t2024, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, base_extractiveness, 2024, 0.19).
narrative_ontology:measurement(vacc_be_t2026, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, base_extractiveness, 2026, 0.19).

% Suppression requirement over time
narrative_ontology:measurement(vacc_su_t2020, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, suppression_requirement, 2020, 0.04).
narrative_ontology:measurement(vacc_su_t2021, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, suppression_requirement, 2021, 0.06).
narrative_ontology:measurement(vacc_su_t2022, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, suppression_requirement, 2022, 0.07).
narrative_ontology:measurement(vacc_su_t2023, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, suppression_requirement, 2023, 0.08).
narrative_ontology:measurement(vacc_su_t2024, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, suppression_requirement, 2024, 0.08).
narrative_ontology:measurement(vacc_su_t2026, vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, suppression_requirement, 2026, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, 0.06).
narrative_ontology:affects_constraint(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, vaccine_mandate_legitimacy__public_health_primacy_reading).
narrative_ontology:affects_constraint(vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, vaccine_mandate_legitimacy__risk_stratification_reading).

% DUAL FORMULATION NOTE:
% The vaccine_mandate_legitimacy kernel admits three structurally distinct readings with incommensurable beneficiary structures and ε values. This story (bodily_autonomy_primacy) treats immunocompromised as victims bearing acceptable externalities. The public_health_primacy_reading treats them as primary beneficiaries warranting protection. The risk_stratification_reading treats them as a calibration case. The three constraints share a kernel (constitutional authority over medical decisions) but differ in foundational axioms (autonomy-priority vs. health-priority vs. proportionality-requirement). Each reading produces a distinct constraint with stable ε; the readings are siblings linked via affects_constraints, not aspects of a single constraint measured from different angles.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
