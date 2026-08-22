% ============================================================================
% CONSTRAINT STORY: imposition_pathway_kernel__hybrid_cascade_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_imposition_pathway_kernel__hybrid_cascade_reading, []).

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
 *   constraint_id: imposition_pathway_kernel__hybrid_cascade_reading
 *   human_readable: State-Mandated Fringe Creation as Commitment Displacement Vector
 *   domain: historical_sociology/state_formation
 *
 * SUMMARY:
 *   The Meiji state issued a top-down decree mandating commitment adoption
 *   (Western dress, official language, bureaucratic hierarchy) among state
 *   employees and military personnel. This mandate created an artificial
 *   fringe — agents required to adopt regardless of preference. Over the
 *   following decades, civilian populations observed this fringe adoption and
 *   gradually perceived the commitment form as legitimate, modern, and
 *   organically chosen. Elite and merchant classes adopted voluntarily,
 *   completing a cascade. The hybrid reading argues that state override
 *   initiated the displacement, but fringe adoption and cascade completed it:
 *   the mechanism is neither purely endogenous climb nor purely exogenous
 *   override, but a sequence where override manufactures the fringe and the
 *   fringe becomes the climb vector. The ε-invariance principle fixes the
 *   referent: the standing arrangement under contest is the state-mandated
 *   adoption system, assessed by the hybrid reading's own lights as
 *   substantially extractive (cost borne by populations without consent).
 *
 * KEY AGENTS:
 *   - state_administrative_apparatus: Agenda-setter, institutional power — issues decree, manufactures mandatory fringe
 *   - military_institution: Beneficiary and identity-locked payer — mandated to adopt, gains legitimacy from adoption
 *   - state_employees: Payer with identity-lock — bear adoption costs; their visible adoption signals legitimacy to civilians
 *   - domain_populations: Powerless payers — observe fringe adoption; gradually perceive commitment as organic; voluntary adoption completes cascade
 *   - observing_elite: Mobile beneficiaries — watch state employees, then voluntarily adopt for career/status gain
 *   - rival_institutional_forms: Excluded and eroded — trapped by the displacement, no voice in decree
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(imposition_pathway_kernel__hybrid_cascade_reading, 0.68).
domain_priors:suppression_score(imposition_pathway_kernel__hybrid_cascade_reading, 0.72).
domain_priors:theater_ratio(imposition_pathway_kernel__hybrid_cascade_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(imposition_pathway_kernel__hybrid_cascade_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(imposition_pathway_kernel__hybrid_cascade_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(imposition_pathway_kernel__hybrid_cascade_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(imposition_pathway_kernel__hybrid_cascade_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(imposition_pathway_kernel__hybrid_cascade_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(imposition_pathway_kernel__hybrid_cascade_reading, tangled_rope).
narrative_ontology:human_readable(imposition_pathway_kernel__hybrid_cascade_reading, "State-Mandated Fringe Creation as Commitment Displacement Vector").
narrative_ontology:topic_domain(imposition_pathway_kernel__hybrid_cascade_reading, "historical_sociology/state_formation").

domain_priors:requires_active_enforcement(imposition_pathway_kernel__hybrid_cascade_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(imposition_pathway_kernel__hybrid_cascade_reading, 'b9eaa0a4-d80a-4315-bf9b-d9a9c8caab78').
narrative_ontology:cs_kernel_codification('b9eaa0a4-d80a-4315-bf9b-d9a9c8caab78', formalized).
narrative_ontology:cs_authority_grounding('b9eaa0a4-d80a-4315-bf9b-d9a9c8caab78', extraction).
narrative_ontology:cs_interpretation_layer_present('b9eaa0a4-d80a-4315-bf9b-d9a9c8caab78').
narrative_ontology:cs_reading_relation('b9eaa0a4-d80a-4315-bf9b-d9a9c8caab78', imposition_pathway_kernel__endogenous_climb_reading, influences).
narrative_ontology:cs_reading_relation('b9eaa0a4-d80a-4315-bf9b-d9a9c8caab78', imposition_pathway_kernel__exogenous_override_reading, coexists_with).
narrative_ontology:cs_axiom('b9eaa0a4-d80a-4315-bf9b-d9a9c8caab78', foundational, state_override_manufactures_adoption_fringe).
narrative_ontology:cs_axiom_status(state_override_manufactures_adoption_fringe, holdable).
narrative_ontology:cs_axiom_grounding('b9eaa0a4-d80a-4315-bf9b-d9a9c8caab78', state_override_manufactures_adoption_fringe, empirically_contingent).
narrative_ontology:cs_axiom('b9eaa0a4-d80a-4315-bf9b-d9a9c8caab78', foundational, fringe_adoption_enables_cascade_to_populations).
narrative_ontology:cs_axiom_status(fringe_adoption_enables_cascade_to_populations, holdable).
narrative_ontology:cs_axiom_grounding('b9eaa0a4-d80a-4315-bf9b-d9a9c8caab78', fringe_adoption_enables_cascade_to_populations, empirically_contingent).
narrative_ontology:cs_reference_frame('b9eaa0a4-d80a-4315-bf9b-d9a9c8caab78', unified_institutional_form_meiji_framing).
narrative_ontology:cs_drift_state('b9eaa0a4-d80a-4315-bf9b-d9a9c8caab78', contemporary_institutional_normalization, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('b9eaa0a4-d80a-4315-bf9b-d9a9c8caab78', '').
narrative_ontology:cs_kernel_id(imposition_pathway_kernel__hybrid_cascade_reading, imposition_pathway_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(imposition_pathway_kernel__hybrid_cascade_reading, state_administrative_apparatus).
narrative_ontology:constraint_beneficiary(imposition_pathway_kernel__hybrid_cascade_reading, military_institution).
narrative_ontology:constraint_victim(imposition_pathway_kernel__hybrid_cascade_reading, domain_populations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(imposition_pathway_kernel__hybrid_cascade_reading, state_employees).
narrative_ontology:constraint_beneficiary(imposition_pathway_kernel__hybrid_cascade_reading, observing_elite).
narrative_ontology:constraint_victim(imposition_pathway_kernel__hybrid_cascade_reading, military_institution).
narrative_ontology:constraint_victim(imposition_pathway_kernel__hybrid_cascade_reading, state_employees).
narrative_ontology:constraint_victim(imposition_pathway_kernel__hybrid_cascade_reading, rival_institutional_forms).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issues top-down decree mandating commitment adoption (e.g., Meiji decree on Western dress, language, institutional forms). The state positions this as modernization/rationalization. State employees and military personnel become the mandatory-adoption fringe; their enforced adoption then legitimizes the commitment as 'natural' or 'inevitable' to civilian populations watching the model.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__hybrid_cascade_reading, state_administrative_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).

% Required by decree to adopt the new commitment form (dress, language, hierarchy structure). Simultaneously benefits: the new form increases operational coherence, state-sponsored legitimacy, and institutional prestige. Enforcement of the decree on military personnel is internal discipline; the military then becomes the visible vanguard, making the commitment appear organic to civilian populations.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__hybrid_cascade_reading, military_institution, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(imposition_pathway_kernel__hybrid_cascade_reading, military_institution, payer).

% Mandated by decree to adopt the commitment form (dress, language, institutional practices). Non-adoption means loss of position, which is identity-constitutive (career path dependence, professional identity fusion). They are the artificial fringe the decree creates. Their visible adoption then signals to civilian populations that the commitment is the legitimate/modern way to organize.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__hybrid_cascade_reading, state_employees, payer,
    moderate, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(imposition_pathway_kernel__hybrid_cascade_reading, state_employees, beneficiary).

% Observe the state employees and military adopting the new commitment form. Over time, the form appears as the coordinated choice of influential peers and legitimate authority, making the same adoption attractive/inevitable. Resistance to adoption softens when the form is perceived as organically climbed rather than externally imposed. The populations ultimately pay through adoption costs (learning, cultural displacement, identity adjustment), but the suppression mechanism appears as social proof rather than state coercion.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__hybrid_cascade_reading, domain_populations, payer,
    powerless, biographical, constrained, national).

% Pre-existing commitments (feudal hierarchy, domain-based identity, indigenous language protocols) are structurally incompatible with the newly mandated form. They are excluded from the decree's scope but eroded by the mandated form's expansion. No voice in the displacement decision; trapped by it.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__hybrid_cascade_reading, rival_institutional_forms, excluded,
    moderate, generational, trapped, national).
narrative_ontology:stakeholder_secondary_role(imposition_pathway_kernel__hybrid_cascade_reading, rival_institutional_forms, payer).

% Non-mandated sector (merchants, intellectuals, regional authorities) initially watch the state employees adopt. Over time, they perceive adoption as culturally advantageous (access to state networks, international legitimacy, career advancement). They voluntarily adopt the commitment form, completing the organic climb. The state decree created the initial fringe; elite voluntary adoption completes the cascade.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__hybrid_cascade_reading, observing_elite, beneficiary,
    organized, biographical, mobile, national).

% Examine the displacement mechanism: did the commitment spread because of organic adoption pressure, or because state coercion was effective? The hybrid reading argues both: state decree manufactured the initial fringe; the fringe then became a cascade vector; climb completed what override initiated.
narrative_ontology:constraint_stakeholder(imposition_pathway_kernel__hybrid_cascade_reading, historical_observers, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(imposition_pathway_kernel__hybrid_cascade_reading, state_administrative_apparatus).
narrative_ontology:fixing_cost_class(imposition_pathway_kernel__hybrid_cascade_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aligns state institutions (military, bureaucracy, diplomatic corps) with a unified symbolic and operational form (dress, language, hierarchy structure), creating operational coherence across dispersed state agencies and signaling state modernity to international and domestic audiences.
% TRANSFER_FUNCTION: Moves adoption costs (time, identity-adjustment, cultural displacement) from state-apparatus beneficiaries to domain populations. The state decree concentrates adoption cost onto state employees and military (identity-locked exit); their visible adoption then shifts the perception to 'organic climb' and reduces perceived coercion on civilian populations, who then voluntarily pay similar costs because the form appears chosen rather than imposed.
% ABSENT_VOICES: Rival institutional forms and pre-existing commitment holders (feudal hierarchy, domain elders, indigenous institutional authorities) are structurally excluded from the decree process. They would attest that the displacement is coercive elimination, not coordination; their voices are suppressed by the state monopoly on decree authority.
% DISAPPEARANCE_RATIONALE: If the state decree and its enforcement machinery vanished, the mandatory adoption by state employees and military would cease; the fringe would collapse; and the cascade to civilian populations would slow or reverse. The commitment form might persist in state institutions (path dependence), but without the decree, it would not have spread to the domain populations as completely or as rapidly. The displacement mechanism depends on the initial state override.
% FOUNDING_PROBLEM: State modernization required unified institutional form across dispersed agencies to enable coordination and legitimacy on the international stage. Pre-existing domain-specific commitments (feudal hierarchy, regional languages, localized dress) fragmented state capacity and signaled institutional weakness.
% FOUNDING_PROBLEM_CORROBORATION: State historians and modernization theorists attest the founding problem was live and required institutional unification. Historians of displaced domains and rival institutional forms attest the 'modernization' framing was a cover story for state consolidation and cultural erasure; they dispute whether the problem required the specific form of coercive unification used. No corroboration from the mandate-excluded populations (they were not consulted).
narrative_ontology:disappearance_verdict(imposition_pathway_kernel__hybrid_cascade_reading, world_rearranges).
narrative_ontology:founding_problem_status(imposition_pathway_kernel__hybrid_cascade_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(imposition_pathway_kernel__hybrid_cascade_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(imposition_pathway_kernel__hybrid_cascade_reading, 'none', 1).
narrative_ontology:epsilon_provenance(imposition_pathway_kernel__hybrid_cascade_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(imposition_pathway_kernel__hybrid_cascade_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(imposition_pathway_kernel__hybrid_cascade_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(imposition_pathway_kernel__hybrid_cascade_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.35 at decree initiation to 0.68 by cascade completion because the costs are borne by populations who did not consent to the displacement. The decree itself is coercive (suppression starts at 0.88); suppression requirement declines as the fringe adoption makes the commitment appear voluntary (theater ratio rises from 0.08 to 0.41 in the early-to-middle period). By t=60, extractiveness stabilizes around 0.62-0.68: the commitment has become institutionalized, but the initial coercion is no longer visible. Theater ratio peaks mid-cascade (t=20-30) when the fringe is most visible but before elite voluntary adoption completes; it declines slightly as the commitment becomes normalized and the theatrical performance of legitimacy is less necessary. Suppression requirement declines throughout because once the cascade completes, enforcement needs shift from overt state coercion to internalized identity-lock and social proof. The measurement grid documents the hybrid pathway: override initiates (high suppression, low theater), fringe adoption cascades (theater rises, suppression declines), climb completes (extraction stabilizes at moderate level, suppression becomes internalized).
 *
 * PERSPECTIVAL GAP:
 *   The state apparatus views the decree as enabling genuine coordination (unified institutions = operational coherence). Domain populations view it as extraction without consent (adoption cost, identity displacement). The gap narrows over time because the fringe adoption makes the commitment appear organically chosen; but the gap never fully closes because the initial coercion was required to initiate the cascade.
 *
 * DIRECTIONALITY LOGIC:
 *   The state apparatus and military benefit from the decree (institutional coherence, international legitimacy, operational efficiency) — their d values near the beneficiary end (0.1-0.3). State employees are trapped (identity-locked exit): mandatory adoption with no acceptable exit option — d near the target end (0.75-0.85). Domain populations are powerless with constrained exit: adoption is advantageous in state-dominated economy, but refusal carries social and economic cost — d in the middle-to-high range (0.55-0.75). The observing elite have mobile exit: they can adopt or not without state penalty, so their d is moderate (0.3-0.5). The directionality spread drives the per-seat type divergence: the state sees rope; the populations see snare/tangled_rope.
 *
 * MANDATROPHY ANALYSIS:
 *   The hybrid reading rejects pure mandatrophy (mandate outlived by function) because both the coordination function (institutional modernization) and the extraction mechanism (delegitimizing rival forms) remain live at t=60. However, the reading identifies a functional shift: the decree is essential at t=0-10 (suppression high, extractiveness low); by t=30-60, the fringe adoption and cascade reduce reliance on state coercion (suppression declines), but the commitment persists as identity-locked and socially enforced rather than state-enforced. The mandate has not become mandatrophy; it has been internalized. This is the hybrid reading's core insight: override initiates, fringe adoption shifts the enforcement mechanism, and climb completes by making the commitment appear organic.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    suppression_internalization,
    'Is the decline in measured suppression from t=0 to t=60 because the state reduced enforcement intensity, or because targets internalized the commitment and enforcement became invisible (identity-lock)?',
    'Post-withdrawal evidence: if state enforcement withdrawal (decree rescission, institution collapse) leads to persistence of the commitment form, the suppression is internalized. If withdrawal leads to rapid abandonment, the suppression was structural and is now internalized.',
    'If suppression is internalized, the constraint''s effective extraction is higher than the measured suppression suggests — targets carry the enforcement cost internally. This supports the snare reading over the rope reading for domain-population seats. If suppression is truly reduced (state enforcement relaxed), the commitment has become self-sustaining through social proof alone.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization, empirical, 'Whether declining suppression reflects enforcement reduction or internalization.').

omega_variable(
    fringe_necessity_for_cascade,
    'Was the state-manufactured fringe (mandatory adoption by state employees and military) necessary to complete the cascade to domain populations, or would the cascade have occurred without the fringe?',
    'Comparative-institutional analysis: examine cases where top-down decree was issued WITHOUT mandatory fringe adoption (decree stated preference but did not mandate for officials). If cascade completion rates differ from fringe cases, the fringe is necessary; if equivalent, the cascade is decoupled from fringe.',
    'If the fringe is necessary, the hybrid reading is correct: override initiates by creating fringe, and fringe completes the cascade. If fringe is unnecessary, the exogenous_override_reading (override is a distinct mechanism) or endogenous_climb_reading (the fringe was already present, hidden) becomes more plausible.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(fringe_necessity_for_cascade, empirical, 'Whether state-manufactured fringe is a necessary condition for cascade completion.').

omega_variable(
    rival_form_displacement_mechanism,
    'Did the rival institutional forms (feudal hierarchy, domain identity, indigenous language) become explicitly incompatible with the new form, or gradually eroded as the new form dominated social networks?',
    'Historical analysis of institutional conflict: did the state decree explicitly outlaw rival forms (incompatibility by decree), or did rival forms fade as career/status advantages shifted to the new form (incompatibility by incentive).',
    'If explicit incompatibility, the constraint is pure coercive displacement (snare). If incentive-driven fading, the constraint is extraction of adoption costs from populations, but the displacement appears organic. The hybrid reading assumes both: explicit decree incompatibility (override) + incentive-driven cascading adoption (climb).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rival_form_displacement_mechanism, empirical, 'Whether rival-form displacement is coercive or incentive-driven.').

omega_variable(
    kernel_reading_alternative_exogenous,
    'Could the same historical sequence be read as pure exogenous override (state capacity alone, no fringe adoption pathway necessary), with the fringe adoption as secondary theater?',
    'Mechanistic model: can the observed adoption pattern be explained by state monopoly on force and authority without invoking fringe adoption as a cascade trigger? If state enforcement alone predicts adoption rates, the exogenous reading is sufficient.',
    'If exogenous override alone explains the data, the constraint is in the exogenous_override_reading, not hybrid. If fringe adoption independently predicts adoption beyond state enforcement, the hybrid reading is supported. This omega documents the reading-level uncertainty.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_alternative_exogenous, conceptual, 'Whether the hybrid cascade mechanism is structurally necessary or exogenous override suffices.').

omega_variable(
    kernel_reading_alternative_endogenous,
    'Could the same historical sequence be read as pure endogenous climb (the decree was rationalization of a climb already underway, not initiation of it), with the state-manufactured fringe as an invisible earlier stage?',
    'Pre-decree evidence: examine whether the commitment form had adopters among state employees, military, or elite BEFORE the decree. If adoption is detectable pre-decree, the decree codifies an existing climb, not the initiation. If adoption is purely post-decree, the decree initiated it.',
    'If pre-decree adoption is detectable, the endogenous_climb_reading is plausible: the fringe was already present (hidden), and the decree accelerated organic climb. If adoption is purely post-decree, the hybrid reading (override manufactures the fringe from which climb begins) is correct.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_alternative_endogenous, empirical, 'Whether the state decree initiated fringe adoption or codified pre-existing climb.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(imposition_pathway_kernel__hybrid_cascade_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(impo_tr_t0, imposition_pathway_kernel__hybrid_cascade_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(impo_tr_t0, projected).
narrative_ontology:measurement(impo_tr_t10, imposition_pathway_kernel__hybrid_cascade_reading, theater_ratio, 10, 0.22).
narrative_ontology:measurement_basis(impo_tr_t10, observed).
narrative_ontology:measurement(impo_tr_t20, imposition_pathway_kernel__hybrid_cascade_reading, theater_ratio, 20, 0.36).
narrative_ontology:measurement_basis(impo_tr_t20, observed).
narrative_ontology:measurement(impo_tr_t30, imposition_pathway_kernel__hybrid_cascade_reading, theater_ratio, 30, 0.41).
narrative_ontology:measurement_basis(impo_tr_t30, observed).
narrative_ontology:measurement(impo_tr_t45, imposition_pathway_kernel__hybrid_cascade_reading, theater_ratio, 45, 0.39).
narrative_ontology:measurement_basis(impo_tr_t45, observed).
narrative_ontology:measurement(impo_tr_t60, imposition_pathway_kernel__hybrid_cascade_reading, theater_ratio, 60, 0.36).
narrative_ontology:measurement_basis(impo_tr_t60, observed).

% Extraction over time
narrative_ontology:measurement(impo_be_t0, imposition_pathway_kernel__hybrid_cascade_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement_basis(impo_be_t0, projected).
narrative_ontology:measurement(impo_be_t10, imposition_pathway_kernel__hybrid_cascade_reading, base_extractiveness, 10, 0.52).
narrative_ontology:measurement_basis(impo_be_t10, observed).
narrative_ontology:measurement(impo_be_t20, imposition_pathway_kernel__hybrid_cascade_reading, base_extractiveness, 20, 0.64).
narrative_ontology:measurement_basis(impo_be_t20, observed).
narrative_ontology:measurement(impo_be_t30, imposition_pathway_kernel__hybrid_cascade_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement_basis(impo_be_t30, observed).
narrative_ontology:measurement(impo_be_t45, imposition_pathway_kernel__hybrid_cascade_reading, base_extractiveness, 45, 0.66).
narrative_ontology:measurement_basis(impo_be_t45, observed).
narrative_ontology:measurement(impo_be_t60, imposition_pathway_kernel__hybrid_cascade_reading, base_extractiveness, 60, 0.62).
narrative_ontology:measurement_basis(impo_be_t60, observed).

% Suppression requirement over time
narrative_ontology:measurement(impo_su_t0, imposition_pathway_kernel__hybrid_cascade_reading, suppression_requirement, 0, 0.88).
narrative_ontology:measurement_basis(impo_su_t0, projected).
narrative_ontology:measurement(impo_su_t10, imposition_pathway_kernel__hybrid_cascade_reading, suppression_requirement, 10, 0.79).
narrative_ontology:measurement_basis(impo_su_t10, observed).
narrative_ontology:measurement(impo_su_t20, imposition_pathway_kernel__hybrid_cascade_reading, suppression_requirement, 20, 0.74).
narrative_ontology:measurement_basis(impo_su_t20, observed).
narrative_ontology:measurement(impo_su_t30, imposition_pathway_kernel__hybrid_cascade_reading, suppression_requirement, 30, 0.72).
narrative_ontology:measurement_basis(impo_su_t30, observed).
narrative_ontology:measurement(impo_su_t45, imposition_pathway_kernel__hybrid_cascade_reading, suppression_requirement, 45, 0.68).
narrative_ontology:measurement_basis(impo_su_t45, observed).
narrative_ontology:measurement(impo_su_t60, imposition_pathway_kernel__hybrid_cascade_reading, suppression_requirement, 60, 0.62).
narrative_ontology:measurement_basis(impo_su_t60, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(imposition_pathway_kernel__hybrid_cascade_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(imposition_pathway_kernel__hybrid_cascade_reading, 0.14).
narrative_ontology:affects_constraint(imposition_pathway_kernel__hybrid_cascade_reading, imposition_pathway_kernel__endogenous_climb_reading).
narrative_ontology:affects_constraint(imposition_pathway_kernel__hybrid_cascade_reading, imposition_pathway_kernel__exogenous_override_reading).

% DUAL FORMULATION NOTE:
% Three constraints compose the imposition_pathway_kernel kernel family: the hybrid_cascade_reading unifies insights from the endogenous_climb and exogenous_override readings by modeling state override as the mechanism that manufactures the initial fringe, and fringe adoption as the mechanism that cascades the commitment through domain populations. The three readings differ in how they attribute causal weight to top-down decree versus bottom-up adoption, and in how they model the state's role: exogenous_override treats override as independent; endogenous_climb treats override as a rationalization of pre-existing climb; hybrid_cascade treats override and climb as sequential, interdependent stages. Each reading carries its own ε value (the standing arrangement assessed by that reading's own lights) and its own beneficiary/victim structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(imposition_pathway_kernel__hybrid_cascade_reading, powerless, 0.68).
constraint_indexing:directionality_override(imposition_pathway_kernel__hybrid_cascade_reading, organized, 0.42).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
