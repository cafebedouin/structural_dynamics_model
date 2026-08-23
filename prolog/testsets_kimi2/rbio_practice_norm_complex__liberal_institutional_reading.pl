% ============================================================================
% CONSTRAINT STORY: rbio_practice_norm_complex__liberal_institutional_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rbio_practice_norm_complex__liberal_institutional_reading, []).

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
 *   constraint_id: rbio_practice_norm_complex__liberal_institutional_reading
 *   human_readable: RBIO Liberal Institutional Reading: Consent-Based Multilateral Order
 *   domain: international_relations/political_economy
 *
 * SUMMARY:
 *   This constraint story captures the liberal institutional reading of the
 *   Rules-Based International Order (RBIO) kernel. Under this reading, RBIO
 *   norms are universal, consent-based, and revisable through multilateral
 *   process; enforcement gaps reflect capacity limitations rather than
 *   legitimacy deficits. The story treats the standing arrangementâthe
 *   UN-centered institutional order with selective enforcement and P5 veto
 *   structureâas the referent. Key agents include intervening states and
 *   defense contractors who benefit from the legitimizing framework, targeted
 *   states and sanctioned civilian populations who bear the costs, and the UN
 *   Security Council which administers the enforcement agenda. The constraint
 *   is claimed as tangled_rope because it carries a genuine coordination
 *   function (great-power war prevention, trade predictability) alongside
 *   asymmetric extraction (sanctions, intervention rents, conditionality).
 *   Sibling readings (hegemonic extraction, sovereignty maximalist) are
 *   documented in kernel_context and linked via network edges.
 *
 * KEY AGENTS:
 *   - intervening_states: Primary beneficiary (institutional/global) â gain legitimacy and contract flows
 *   - defense_contractors: Secondary beneficiary (powerful/global) â receive intervention rents
 *   - targeted_states: Primary payer (institutional/national) â sovereignty constrained by sanctions
 *   - civilian_populations_under_sanctions: Secondary payer (powerless/national) â bear humanitarian costs
 *   - un_security_council: Agenda setter (institutional/global) â administers enforcement selectively
 *   - global_south_observers: Excluded voice (organized/global) â contest selective universalism
 *   - humanitarian_ngo_observers: Analytical seat (organized/global) â witness and mediate harm
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rbio_practice_norm_complex__liberal_institutional_reading, 0.62).
domain_priors:suppression_score(rbio_practice_norm_complex__liberal_institutional_reading, 0.58).
domain_priors:theater_ratio(rbio_practice_norm_complex__liberal_institutional_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rbio_practice_norm_complex__liberal_institutional_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(rbio_practice_norm_complex__liberal_institutional_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(rbio_practice_norm_complex__liberal_institutional_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rbio_practice_norm_complex__liberal_institutional_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(rbio_practice_norm_complex__liberal_institutional_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rbio_practice_norm_complex__liberal_institutional_reading, tangled_rope).
narrative_ontology:human_readable(rbio_practice_norm_complex__liberal_institutional_reading, "RBIO Liberal Institutional Reading: Consent-Based Multilateral Order").
narrative_ontology:topic_domain(rbio_practice_norm_complex__liberal_institutional_reading, "international_relations/political_economy").

domain_priors:requires_active_enforcement(rbio_practice_norm_complex__liberal_institutional_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rbio_practice_norm_complex__liberal_institutional_reading, '365d8084-6504-4b2e-9def-14a7df740ae6').
narrative_ontology:cs_kernel_codification('365d8084-6504-4b2e-9def-14a7df740ae6', formalized).
narrative_ontology:cs_authority_grounding('365d8084-6504-4b2e-9def-14a7df740ae6', lineage).
narrative_ontology:cs_interpretation_layer_present('365d8084-6504-4b2e-9def-14a7df740ae6').
narrative_ontology:cs_reading_relation('365d8084-6504-4b2e-9def-14a7df740ae6', rbio_practice_norm_complex__hegemonic_extraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('365d8084-6504-4b2e-9def-14a7df740ae6', rbio_practice_norm_complex__sovereignty_maximalist_reading, coexists_with).
narrative_ontology:cs_axiom('365d8084-6504-4b2e-9def-14a7df740ae6', foundational, multilateral_consent_as_legitimacy_source).
narrative_ontology:cs_axiom_status(multilateral_consent_as_legitimacy_source, holdable).
narrative_ontology:cs_axiom_grounding('365d8084-6504-4b2e-9def-14a7df740ae6', multilateral_consent_as_legitimacy_source, conventional).
narrative_ontology:cs_axiom('365d8084-6504-4b2e-9def-14a7df740ae6', foundational, enforcement_selectivity_reflects_capacity).
narrative_ontology:cs_axiom_status(enforcement_selectivity_reflects_capacity, holdable).
narrative_ontology:cs_axiom_grounding('365d8084-6504-4b2e-9def-14a7df740ae6', enforcement_selectivity_reflects_capacity, empirically_contingent).
narrative_ontology:cs_reference_frame('365d8084-6504-4b2e-9def-14a7df740ae6', post_war_multilateral_consensus).
narrative_ontology:cs_drift_state('365d8084-6504-4b2e-9def-14a7df740ae6', post_unipolar_contestation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('365d8084-6504-4b2e-9def-14a7df740ae6', '').
narrative_ontology:cs_kernel_id(rbio_practice_norm_complex__liberal_institutional_reading, rbio_practice_norm_complex).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rbio_practice_norm_complex__liberal_institutional_reading, intervening_states).
narrative_ontology:constraint_beneficiary(rbio_practice_norm_complex__liberal_institutional_reading, defense_contractors).
narrative_ontology:constraint_victim(rbio_practice_norm_complex__liberal_institutional_reading, targeted_states).
narrative_ontology:constraint_victim(rbio_practice_norm_complex__liberal_institutional_reading, civilian_populations_under_sanctions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Lead sanctions regimes and military interventions under UNSC or humanitarian authorization. Benefit from normative legitimacy, preferential contract access, and institutional agenda-setting power. Can selectively participate in or abstain from enforcement without exiting the order.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__liberal_institutional_reading, intervening_states, beneficiary,
    institutional, generational, mobile, global).

% Receive stabilization, logistics, and security contracts tied to RBIO-enforced interventions and sanctions monitoring. Revenue flows are concentrated and directly linked to the enforcement footprint of the order.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__liberal_institutional_reading, defense_contractors, beneficiary,
    powerful, biographical, mobile, global).

% Subject to sanctions, intervention, or conditionality by the RBIO framework. Sovereignty is partially suspended; economic and diplomatic alternatives exist but carry high isolation costs. Exit from the order is possible only by accepting pariah status or aligning with rival blocs.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__liberal_institutional_reading, targeted_states, payer,
    institutional, generational, constrained, national).

% Bear humanitarian and economic costs of sanctions and conflict despite the RBIO's claimed protective purpose. No voice in UNSC decisions that authorize the constraints they live under; emigration is often the only exit and is restricted by borders and resources.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__liberal_institutional_reading, civilian_populations_under_sanctions, payer,
    powerless, immediate, trapped, national).

% Formal gatekeeper for legitimate enforcement. Its decisions authorize sanctions and intervention, but its composition is structurally frozen by the P5 veto, making substantive revision of the rules extremely difficult despite formal amendment procedures.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__liberal_institutional_reading, un_security_council, agenda_setter,
    institutional, generational, constrained, global).

% States and movements that contest the universality of RBIO norms and experience them as imposed conditionality. They call for multipolar institutional alternatives but are marginal to the norm-setting core.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__liberal_institutional_reading, global_south_observers, excluded,
    organized, generational, constrained, global).

% Deliver aid and monitor rights compliance within RBIO frameworks. Witness civilian harm from sanctions and intervention but depend on institutional access and funding, limiting their capacity to contest the constraint's legitimacy directly.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__liberal_institutional_reading, humanitarian_ngo_observers, observer,
    organized, biographical, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(rbio_practice_norm_complex__liberal_institutional_reading, diffuse).
narrative_ontology:fixing_cost_class(rbio_practice_norm_complex__liberal_institutional_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates interstate behavior through shared legal norms, collective security mechanisms, and multilateral institutions to reduce unilateral aggression and provide predictable frameworks for trade, diplomacy, and conflict resolution.
% TRANSFER_FUNCTION: Moves security resources, economic rents, and normative legitimacy from targeted states and their civilian populations to intervening states and defense contractors through authorized sanctions, intervention contracts, and structural conditionality.
% ABSENT_VOICES: States in the Global South that experience RBIO as imposed conditionality rather than consent-based order; civilian populations in targeted states who bear sanction costs without voice in UNSC decisions; non-Western epistemic communities questioning the universalism claim.
% DISAPPEARANCE_RATIONALE: Without RBIO, interstate coercion loses multilateral legitimacy scaffolding; sanctions regimes collapse, intervention narratives revert to raw power politics, and institutional architecture reorganizes around competing regional blocs.
% FOUNDING_PROBLEM: Prevention of great-power war and mitigation of 1930s-style economic nationalism through rules-based institutional frameworks.
% FOUNDING_PROBLEM_CORROBORATION: Diplomatic historians and some realist IR scholars outside the direct beneficiary pool corroborate that the great-power war function has been partially successful; Global South legal scholars and post-colonial theorists attest that the economic sovereignty and equity dimensions were never resolved and the founding problem is mis-specified.
narrative_ontology:disappearance_verdict(rbio_practice_norm_complex__liberal_institutional_reading, world_rearranges).
narrative_ontology:founding_problem_status(rbio_practice_norm_complex__liberal_institutional_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(rbio_practice_norm_complex__liberal_institutional_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(rbio_practice_norm_complex__liberal_institutional_reading, 'none', 1).
narrative_ontology:epsilon_provenance(rbio_practice_norm_complex__liberal_institutional_reading, 0.62, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rbio_practice_norm_complex__liberal_institutional_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(rbio_practice_norm_complex__liberal_institutional_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(rbio_practice_norm_complex__liberal_institutional_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) reflects that sanctions and intervention extract heavily from targeted societies while the coordination benefit (great-power peace) is diffuse. Suppression (0.58) captures the active exclusion of alternatives (P5 veto blocking reform, targeted states unable to exit sanctions). Theater ratio (0.48) measures the gap between the universalist, consent-based legitimacy claim and the observed selectivity of enforcement. Accessibility collapse (0.48) is moderate: alternatives (multipolar arrangements, regional orders) are visible but institutionally marginalized. Resistance (0.55) reflects organized pushback from targeted states and the Global South. The temporal series show extraction and theater rising through the unipolar period and plateauing as multipolar contestation increases.
 *
 * PERSPECTIVAL GAP:
 *   The intervening-state seat experiences the constraint as legitimate coordination it maintains; the targeted-state seat experiences the same structure as coercive extraction. The engine should compute a wide divergence: intervening states as low-d (beneficiary) and targeted states as high-d (target). Civilian populations sit at the extreme target end due to powerlessness and trapped exit. The divergence is structural, not perspectival in the sense of mere opinion.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (intervening_states, defense_contractors) derive legitimacy, security contracts, and policy autonomy from the RBIO framework; their directionality is near the subsidy end. Victims (targeted_states, civilian_populations_under_sanctions) bear sovereignty loss, economic harm, and physical violence filtered through the same institutional structure; their directionality is near the full-target end. The UN Security Council sits ambiguously: it administers the constraint but is itself structurally captured by P5 interests, giving it a moderately low d as an agenda-setter that partly shares in the beneficiary position.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as tangled_rope prevents misreading the RBIO as a pure snare (which would ignore the real coordination function in great-power war prevention and trade stability) or as a pure rope (which would ignore the asymmetric extraction from targeted societies). The mandate has not fully atrophied: the founding problem of great-power war prevention is partially live. However, enforcement selectivity and the rise of intervention rents indicate substantial extraction layered onto the coordination skeleton. The theater ratio captures this layering without collapsing the coordination function entirely.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    selectivity_legitimacy_ambiguity,
    'Is enforcement selectivity a genuine capacity problem, or does it reflect structural bias toward powerful states'' interests?',
    'Comparative case analysis of enforcement initiation and non-initiation across comparable atrocity and breach profiles, controlling for geopolitical alignment and resource constraints.',
    'If selectivity tracks power alignment more than capacity, the constraint''s extraction component is higher than the liberal reading admits and the theater ratio understates the legitimacy gap.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(selectivity_legitimacy_ambiguity, empirical, 'Whether selective enforcement reflects capacity constraints or structural bias').

omega_variable(
    revision_veto_lock,
    'Are formal RBIO amendment procedures practically accessible, or does the P5 veto permanently lock the institutional structure?',
    'Historical analysis of amendment proposals and reform initiatives since 1945; measurement of procedural success rates for proposals opposed by any P5 member.',
    'If the veto creates de facto un-amendability, the ''revisable through legitimate multilateral processes'' claim is performative and the theater ratio should rise.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(revision_veto_lock, empirical, 'Whether multilateral revision is structurally possible or veto-locked').

omega_variable(
    kernel_reading_contest,
    'Which reading of the RBIO kernel best captures the structural reality of the standing arrangement?',
    'Cross-reading comparison of predictive accuracy on enforcement patterns, amendment outcomes, and distribution of costs/benefits; evaluation of which reading''s axioms remain holdable under empirical challenge.',
    'If the hegemonic extraction reading outperforms on prediction, this constraint reclassifies toward snare; if the sovereignty maximalist reading gains institutional traction, the constraint''s legitimacy framework collapses entirely.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Structural contest between kernel readings and their classification implications').

omega_variable(
    consent_coercion_boundary,
    'Is economic conditionality under RBIO frameworks genuinely contractual consent, or is it structurally coerced by asymmetric bargaining power?',
    'Bargaining-power analysis of loan and aid agreements; comparison of terms offered to states with and without strategic importance to intervening powers.',
    'If conditionality is structurally coerced, the consent foundation of this reading is undermined and the directionality of affected states shifts further toward full target.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consent_coercion_boundary, conceptual, 'Whether RBIO conditionality is consensual or coerced').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rbio_practice_norm_complex__liberal_institutional_reading, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rbio_tr_t0, rbio_practice_norm_complex__liberal_institutional_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(rbio_tr_t15, rbio_practice_norm_complex__liberal_institutional_reading, theater_ratio, 15, 0.25).
narrative_ontology:measurement(rbio_tr_t30, rbio_practice_norm_complex__liberal_institutional_reading, theater_ratio, 30, 0.35).
narrative_ontology:measurement(rbio_tr_t45, rbio_practice_norm_complex__liberal_institutional_reading, theater_ratio, 45, 0.45).
narrative_ontology:measurement(rbio_tr_t60, rbio_practice_norm_complex__liberal_institutional_reading, theater_ratio, 60, 0.5).
narrative_ontology:measurement(rbio_tr_t75, rbio_practice_norm_complex__liberal_institutional_reading, theater_ratio, 75, 0.48).

% Extraction over time
narrative_ontology:measurement(rbio_be_t0, rbio_practice_norm_complex__liberal_institutional_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(rbio_be_t15, rbio_practice_norm_complex__liberal_institutional_reading, base_extractiveness, 15, 0.4).
narrative_ontology:measurement(rbio_be_t30, rbio_practice_norm_complex__liberal_institutional_reading, base_extractiveness, 30, 0.48).
narrative_ontology:measurement(rbio_be_t45, rbio_practice_norm_complex__liberal_institutional_reading, base_extractiveness, 45, 0.58).
narrative_ontology:measurement(rbio_be_t60, rbio_practice_norm_complex__liberal_institutional_reading, base_extractiveness, 60, 0.65).
narrative_ontology:measurement(rbio_be_t75, rbio_practice_norm_complex__liberal_institutional_reading, base_extractiveness, 75, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(rbio_su_t0, rbio_practice_norm_complex__liberal_institutional_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(rbio_su_t15, rbio_practice_norm_complex__liberal_institutional_reading, suppression_requirement, 15, 0.35).
narrative_ontology:measurement(rbio_su_t30, rbio_practice_norm_complex__liberal_institutional_reading, suppression_requirement, 30, 0.5).
narrative_ontology:measurement(rbio_su_t45, rbio_practice_norm_complex__liberal_institutional_reading, suppression_requirement, 45, 0.6).
narrative_ontology:measurement(rbio_su_t60, rbio_practice_norm_complex__liberal_institutional_reading, suppression_requirement, 60, 0.65).
narrative_ontology:measurement(rbio_su_t75, rbio_practice_norm_complex__liberal_institutional_reading, suppression_requirement, 75, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rbio_practice_norm_complex__liberal_institutional_reading, global_infrastructure).
narrative_ontology:affects_constraint(rbio_practice_norm_complex__liberal_institutional_reading, rbio_practice_norm_complex__hegemonic_extraction_reading).
narrative_ontology:affects_constraint(rbio_practice_norm_complex__liberal_institutional_reading, rbio_practice_norm_complex__sovereignty_maximalist_reading).

% DUAL FORMULATION NOTE:
% The rbio_practice_norm_complex kernel decomposes into three readings: liberal_institutional (this file), hegemonic_extraction, and sovereignty_maximalist. Each instantiates a structurally distinct constraint with different epsilon, beneficiary/victim structure, and type classification. The liberal reading treats the arrangement as legitimate coordination with capacity gaps; the hegemonic reading treats it as frozen extraction; the sovereignty reading treats it as illegitimate interference.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
